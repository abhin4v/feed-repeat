{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib where

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
import Data.Hashable (Hashable)
import Data.List (sortBy)
import Data.List.Extra (nubOrdOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Data.Time (UTCTime (..), diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Network.URI qualified as URI
import System.Random (randomRIO)
import Text.Atom.Feed qualified as Atom
import Text.Feed.Query qualified as Feed
import Text.Feed.Types qualified as Feed
import Prelude hiding (writeFile)

newtype URL = URL String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Hashable)

data FeedTask = FeedTask
  { sourceFeedUrl :: URL,
    outputFilename :: String,
    saveSourceFeedEntries :: Bool,
    repeatedEntryCount :: Int,
    minimumEntryAgeDays :: Int,
    minRunGapDays :: Int,
    maxEntryCountPerDomain :: Maybe Int
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
      <*> v .:? "minRunGapDays" .!= 1
      <*> v .:? "maxEntryCountPerDomain"

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

feedToAtom :: (MonadIO m, MonadError AppError m) => Feed.Feed -> m Atom.Feed
feedToAtom (Feed.AtomFeed af) = return af
feedToAtom feed = do
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
  return $
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

mergeFeeds :: Atom.Feed -> Atom.Feed -> Atom.Feed
mergeFeeds feed1 feed2 =
  let allEntries = Atom.feedEntries feed1 <> Atom.feedEntries feed2
      sortedEntries = sortBy (comparing (Down . Atom.entryUpdated)) allEntries
      uniqueEntries = nubOrdOn (Feed.getItemLink . Feed.AtomItem) sortedEntries
   in feed1 {Atom.feedEntries = uniqueEntries}

selectEntries :: FeedTask -> [Atom.Entry] -> IO [Atom.Entry]
selectEntries task entries = do
  now <- getCurrentTime
  select now $ filter (isOldEnough now) entries
  where
    minAgeSeconds = fromIntegral task.minimumEntryAgeDays * nominalDay

    isOldEnough currentTime entry =
      case parseDate $ Atom.entryUpdated entry of
        Nothing -> True
        Just entryTime -> diffUTCTime currentTime entryTime >= minAgeSeconds

    computeWeight :: UTCTime -> Atom.Entry -> Double
    computeWeight now entry = case parseDate $ Atom.entryUpdated entry of
      Nothing -> 1
      Just updated ->
        let age = diffUTCTime now updated
         in if age > 0 then exp (realToFrac $ age / (nominalDay * 365)) else 1

    -- A-Res algorithm with per-domain limit
    select now es = do
      keys <- forM es $ \entry -> do
        r <- randomRIO (0, 1)
        return $ log r / computeWeight now entry
      zip es keys
        & sortBy (comparing (Down . snd))
        & map fst
        & limitEntries (fromMaybe maxBound task.maxEntryCountPerDomain) mempty
        & return

    limitEntries maxAllowed sourceCounts =
      foldl' step ((task.repeatedEntryCount, sourceCounts), [])
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
          "%Y-%m-%dT%H:%M:%S%Q%Z",
          rfc822DateFormat,
          "%Y-%m-%d",
          "%Y-%-m-%-d",
          "%a, %d %b %Y %H:%M %Z", -- Mon, 14 Jul 2025 10:30 +0000
          "%d %b %Y %H:%M:%S %Z", -- 17 Jul 2022 00:00:00 GMT
          "%a, %d %B %Y %H:%M:%S %Z" -- Mon, 08 December 2025 11:07:49 +0000
        ]
   in asum $ map (\fmt -> parseTimeM True defaultTimeLocale fmt $ T.unpack ds) formats

tryOrThrow :: (MonadIO m, Exception e1, MonadError e2 m) => (e1 -> e2) -> IO c -> m c
tryOrThrow mkErr = try >>> liftIO >=> mapLeft mkErr >>> liftEither

fromMaybeOrThrow :: (MonadError e m) => e -> Maybe a -> m a
fromMaybeOrThrow err = maybeToRight err >>> liftEither

extractDomain :: T.Text -> Maybe String
extractDomain = T.unpack >>> URI.parseURI >=> URI.uriAuthority >>> fmap URI.uriRegName
