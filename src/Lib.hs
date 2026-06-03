{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib
  ( URL,
    newURL,
    NonNegative,
    newNonNegative,
    FeedTask (..),
    AppError (..),
    feedToAtom,
    mergeFeeds,
    selectEntries,
    mkUuidUrn,
    parseDate,
    tryOrThrow,
    fromMaybeOrThrow,
    extractDomain,
  )
where

import Control.Applicative (asum, (<|>))
import Control.Arrow ((>>>))
import Control.Exception (Exception, IOException, displayException, try)
import Control.Monad (forM, (>=>))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Function ((&))
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.List (sortBy)
import Data.List.Extra (nubOrdOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..), comparing)
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Time (UTCTime (..), diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Network.HTTP.Client qualified as HTTP
import Network.URI qualified as URI
import System.FilePath
  ( addTrailingPathSeparator,
    dropFileName,
    dropTrailingPathSeparator,
    takeDirectory,
  )
import System.Random (randomRIO)
import Text.Atom.Feed qualified as Atom
import Text.Feed.Query qualified as Feed
import Text.Feed.Types qualified as Feed
import Prelude hiding (writeFile)

newtype URL = URL {unURL :: String}
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

instance HasField "toString" URL String where
  getField = unURL

instance Show URL where
  show = unURL

newURL :: String -> Maybe URL
newURL url = URL . show . HTTP.getUri <$> HTTP.parseRequest url

instance Aeson.FromJSON URL where
  parseJSON = Aeson.withText "String" $ \str ->
    case newURL (T.unpack str) of
      Nothing -> fail $ "Expect an URL, but found: " <> T.unpack str
      Just url -> return url

newtype NonNegative a = NonNegative {unNonNegative :: a}
  deriving stock (Eq)

instance HasField "toNum" (NonNegative a) a where
  getField = unNonNegative

instance (Show a) => Show (NonNegative a) where
  show (NonNegative a) = show a

newNonNegative :: (Ord a, Num a) => a -> Maybe (NonNegative a)
newNonNegative num = if num >= 0 then Just $ NonNegative num else Nothing

instance Aeson.FromJSON (NonNegative Int) where
  parseJSON = Aeson.withScientific "number" $ \num ->
    if num >= 0
      then case Scientific.toBoundedInteger num of
        Just n -> return $ NonNegative n
        Nothing -> fail $ "Expected non-negative int, but found: " <> show num
      else fail $ "Expected non-negative int, but found: " <> show num

instance Aeson.FromJSON (NonNegative Double) where
  parseJSON = Aeson.withScientific "number" $ \num ->
    if num >= 0
      then case Scientific.toBoundedRealFloat num of
        Right n -> return $ NonNegative n
        Left _ -> fail $ "Expected non-negative double, but found: " <> show num
      else fail $ "Expected non-negative double, but found: " <> show num

data FeedTask = FeedTask
  { sourceFeedUrl :: URL,
    outputFilename :: String,
    saveSourceFeedEntries :: Bool,
    repeatedEntryCount :: NonNegative Int,
    minimumEntryAgeDays :: NonNegative Int,
    minRunGapDays :: NonNegative Int,
    maxEntryCountPerDomain :: Maybe (NonNegative Int),
    selectionAlpha :: NonNegative Double,
    passthroughNewEntries :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON FeedTask where
  parseJSON = Aeson.withObject "FeedTask" $ \v ->
    FeedTask
      <$> v .: "sourceFeedUrl"
      <*> v .: "outputFilename"
      <*> v .: "saveSourceFeedEntries"
      <*> v .: "repeatedEntryCount"
      <*> v .: "minimumEntryAgeDays"
      <*> v .:? "minRunGapDays" .!= NonNegative 1
      <*> v .:? "maxEntryCountPerDomain"
      <*> v .:? "selectionAlpha" .!= NonNegative 1
      <*> v .:? "passthroughNewEntries" .!= False

data AppError
  = IOError IOException
  | FeedParseError FilePath
  | FeedRenderError FilePath
  | InvalidFormatError String FilePath
  | InvalidFeedUpdatedError
  | FeedNotModifiedError
  | HTTPError HTTP.HttpException

instance Show AppError where
  show = \case
    IOError err -> "Failed to read/write file " <> displayException err
    FeedParseError filePath -> "Failed to parse: " <> filePath
    FeedRenderError filePath -> "Failed to render: " <> filePath
    InvalidFormatError format filePath -> "File is not in " <> format <> " format: " <> filePath
    InvalidFeedUpdatedError -> "Feed updated date absent"
    FeedNotModifiedError -> "Feed not modified"
    HTTPError err -> "HTTP error: " <> displayException err

feedToAtom :: (MonadIO m, MonadError AppError m) => URL -> Feed.Feed -> m Atom.Feed
feedToAtom feedURL (Feed.AtomFeed af@Atom.Feed {feedLinks, feedEntries}) =
  return $
    af
      { Atom.feedLinks = map (normalizeLink feedURL) feedLinks,
        Atom.feedEntries =
          map
            (\entry@Atom.Entry {entryLinks} -> entry {Atom.entryLinks = map (normalizeLink feedURL) entryLinks})
            feedEntries
      }
feedToAtom feedURL feed = do
  feedUuid <- mkUuidUrn
  now <- liftIO getCurrentTime
  entries <-
    sortBy (comparing (Down . Atom.entryUpdated))
      <$> traverse (itemToAtomEntry now) (Feed.getFeedItems feed)
  let title = Feed.getFeedTitle feed
      link = Feed.getFeedHome feed
      updateDate = convertDate $ Feed.getFeedLastUpdate feed
      pubDate = convertDate $ Feed.getFeedPubDate feed
      feedId = fromMaybe feedUuid link
      mFeedUpdated = updateDate <|> pubDate <|> listToMaybe (map Atom.entryUpdated entries)

  feedUpdated <- fromMaybeOrThrow InvalidFeedUpdatedError mFeedUpdated
  feedToAtom feedURL . Feed.AtomFeed $
    (Atom.nullFeed feedId (Atom.TextString title) feedUpdated)
      { Atom.feedEntries = entries,
        Atom.feedAuthors =
          maybeToList . fmap (\name -> Atom.nullPerson {Atom.personName = name}) $
            Feed.getFeedAuthor feed,
        Atom.feedCategories =
          map (\(term, scheme) -> (Atom.newCategory term) {Atom.catScheme = scheme}) $
            Feed.getFeedCategories feed,
        Atom.feedLogo = Feed.getFeedLogoLink feed,
        Atom.feedGenerator = Atom.nullGenerator <$> Feed.getFeedGenerator feed,
        Atom.feedLinks = mkLinks [("self", link), ("alternate", Feed.getFeedHTML feed)]
      }
  where
    convertDate md = T.pack . iso8601Show <$> (md >>= parseDate)

    mkLink rel url = (Atom.nullLink url) {Atom.linkRel = Just $ Left rel}
    mkLinks = mapMaybe (\(rel, mUrl) -> mkLink rel <$> mUrl)

    itemToAtomEntry now item = case item of
      Feed.AtomItem atomEntry -> return atomEntry
      _ -> do
        entryUuid <- mkUuidUrn
        let title = Feed.getItemTitle item
            itemId = snd <$> Feed.getItemId item
            link = Feed.getItemLink item
            pubDate = Feed.getItemPublishDateString item >>= parseDate
            entryId = fromMaybe entryUuid (itemId <|> link)
            entryTitle = Atom.TextString $ fromMaybe "" title
            entryUpdated = T.pack $ iso8601Show $ fromMaybe now pubDate
        return
          (Atom.nullEntry entryId entryTitle entryUpdated)
            { Atom.entryAuthors =
                maybeToList . fmap (\name -> Atom.nullPerson {Atom.personName = name}) $
                  Feed.getItemAuthor item,
              Atom.entryCategories = map Atom.newCategory $ Feed.getItemCategories item,
              Atom.entryContent = Atom.HTMLContent <$> Feed.getItemContent item,
              Atom.entryLinks =
                mkLinks [("alternate", link), ("replies", Feed.getItemCommentLink item)]
                  <> maybeToList
                    ( ( \(url, typ, len) ->
                          (mkLink "enclosure" url)
                            { Atom.linkType = typ,
                              Atom.linkLength = T.pack . show <$> len
                            }
                      )
                        <$> Feed.getItemEnclosure item
                    ),
              Atom.entryPublished = Just entryUpdated,
              Atom.entryRights = Atom.HTMLString <$> Feed.getItemRights item,
              Atom.entrySummary = Atom.HTMLString <$> Feed.getItemSummary item
            }

normalizeLink :: URL -> Atom.Link -> Atom.Link
normalizeLink feedUrl link@Atom.Link {linkHref} =
  link {Atom.linkHref = normalizeLinkURL feedUrl linkHref}
  where
    normalizeLinkURL (URL feedUrl) link
      | T.null link = link
      | ("http://" `T.isPrefixOf` link || "https://" `T.isPrefixOf` link)
          && not (isLocalhost link) =
          T.pack $ URI.escapeURIString URI.isAllowedInURI $ T.unpack link
      | Just feedUri <- URI.parseURI feedUrl,
        Just linkUri <- URI.parseURIReference (T.unpack link) =
          let baseUri = feedUri {URI.uriPath = dirPath $ URI.uriPath feedUri}
           in T.pack $ show $ linkUri `URI.relativeTo` baseUri
      | otherwise = link

    isLocalhost link =
      any
        (`T.isPrefixOf` link)
        [s <> h | s <- ["http://", "https://"], h <- ["localhost", "127.0.0.1", "[::1]"]]

    dirPath =
      dropFileName >>> dropTrailingPathSeparator >>> takeDirectory >>> addTrailingPathSeparator

mergeFeeds :: Atom.Feed -> Atom.Feed -> Atom.Feed
mergeFeeds feed1 feed2 =
  let allEntries = Atom.feedEntries feed1 <> Atom.feedEntries feed2
      sortedEntries = sortBy (comparing (Down . Atom.entryUpdated)) allEntries
      uniqueEntries = nubOrdOn getItemLinkOrId sortedEntries
   in feed1 {Atom.feedEntries = uniqueEntries}

selectEntries :: (MonadIO m) => FeedTask -> UTCTime -> [Atom.Entry] -> m ([Atom.Entry], [Atom.Entry])
selectEntries task outputFeedUpdated entries = do
  let newEntries =
        if task.passthroughNewEntries
          then
            [ entry
            | entry <- entries,
              Just pubDate <- [Atom.entryPublished entry],
              Just pubTime <- [parseDate pubDate],
              pubTime >= outputFeedUpdated
            ]
          else []
      newEntryLinks = HS.fromList $ map getItemLinkOrId newEntries

  now <- liftIO getCurrentTime
  fmap ((,newEntries) . filter (not . (`HS.member` newEntryLinks) . getItemLinkOrId))
    . select now
    $ filter (isOldEnough now) entries
  where
    minAgeSeconds = fromIntegral task.minimumEntryAgeDays.toNum * nominalDay

    isOldEnough currentTime entry =
      case parseDate $ Atom.entryUpdated entry of
        Nothing -> True
        Just entryTime -> diffUTCTime currentTime entryTime >= minAgeSeconds

    computeWeight :: UTCTime -> Atom.Entry -> Double
    computeWeight now entry = case parseDate $ Atom.entryUpdated entry of
      Nothing -> 1
      Just updated
        | let age = diffUTCTime now updated,
          age > 0 ->
            exp (task.selectionAlpha.toNum * realToFrac (age / (nominalDay * 365)))
      _ -> 1

    -- A-Res algorithm with per-domain limit
    select now es = do
      keys <- forM es $ \entry -> do
        r <- randomRIO (1e-12, 1)
        return $ log r / computeWeight now entry
      zip es keys
        & sortBy (comparing (Down . snd))
        & map fst
        & limitEntries (maybe maxBound unNonNegative task.maxEntryCountPerDomain) mempty
        & return

    limitEntries maxAllowed sourceCounts =
      foldl' step ((task.repeatedEntryCount.toNum, sourceCounts), [])
        >>> snd
        >>> reverse
      where
        step ((0, counts), acc) _ = ((0, counts), acc)
        step ((rem, counts), acc) e =
          case Feed.getItemLink (Feed.AtomItem e) >>= extractDomain of
            Nothing -> ((rem, counts), acc) -- Skip entries without valid domain
            Just domain
              | let count = HM.lookupDefault 0 domain counts ->
                  if count < maxAllowed
                    then ((rem - 1, HM.insert domain (count + 1) counts), e : acc)
                    else ((rem, counts), acc)

mkUuidUrn :: (MonadIO m) => m T.Text
mkUuidUrn = T.pack . ("urn:uuid:" <>) . show <$> liftIO UUID.nextRandom

parseDate :: T.Text -> Maybe UTCTime
parseDate ds =
  let formats =
        [ "%Y-%m-%dT%H:%M:%S%Z",
          "%Y-%m-%dT%H:%M:%S%EZ",
          "%Y-%m-%dT%H:%M:%SZ",
          "%Y-%m-%dT%H:%M:%S%Q%Z",
          rfc822DateFormat,
          "%Y-%m-%d",
          "%Y-%-m-%-d",
          "%a, %d %b %Y %H:%M %Z", -- Mon, 14 Jul 2025 10:30 +0000
          "%d %b %Y %H:%M:%S %Z", -- 17 Jul 2022 00:00:00 GMT
          "%a, %d %B %Y %H:%M:%S %Z", -- Mon, 08 December 2025 11:07:49 +0000
          "%a, %d %b %Y %H:%M %EZ", -- Mon, 14 Jul 2025 10:30 +00:00
          "%d %b %Y %H:%M:%S %EZ", -- 17 Jul 2022 00:00:00 +00:00
          "%a, %d %B %Y %H:%M:%S %EZ" -- Mon, 08 December 2025 11:07:49 +00:00
        ]
   in asum $ map (\fmt -> parseTimeM True defaultTimeLocale fmt $ T.unpack ds) formats

getItemLinkOrId :: Atom.Entry -> T.Text
getItemLinkOrId entry =
  let link = Feed.getItemLink $ Feed.AtomItem entry
   in fromMaybe (Atom.entryId entry) link

tryOrThrow :: (MonadIO m, Exception e1, MonadError e2 m) => (e1 -> e2) -> IO c -> m c
tryOrThrow mkErr = try >>> liftIO >=> mapLeft mkErr >>> liftEither

fromMaybeOrThrow :: (MonadError e m) => e -> Maybe a -> m a
fromMaybeOrThrow err = maybeToRight err >>> liftEither

extractDomain :: T.Text -> Maybe String
extractDomain = T.unpack >>> URI.parseURI >=> URI.uriAuthority >>> fmap URI.uriRegName
