{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module FeedRepeat.Lib
  ( checkPublicUrl,
    feedToAtom,
    mergeFeeds,
    selectEntries,
    computeNewEntries,
    mkUuidUrn,
    parseDate,
    tryOrThrow,
    fromMaybeOrThrow,
    extractDomain,
    getItemLinkOrId,
  )
where

import Control.Applicative (asum, (<|>))
import Control.Arrow ((>>>))
import Control.Exception (Exception, try)
import Control.Monad (forM, (>=>))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Function ((&))
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (sortBy)
import Data.List.Extra (nubOrdOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Data.Time (UTCTime (..), diffUTCTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID.V4 qualified as UUID
import FeedRepeat.Lib.SSRF (checkPublicUrl)
import FeedRepeat.Types
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

feedToAtom :: (MonadIO m, MonadError AppError m) => UTCTime -> URL -> Feed.Feed -> m Atom.Feed
feedToAtom _ feedURL (Feed.AtomFeed af@Atom.Feed {feedLinks, feedEntries}) =
  return $
    af
      { Atom.feedLinks = map (normalizeLink feedURL) feedLinks,
        Atom.feedEntries =
          map
            (\entry@Atom.Entry {entryLinks} -> entry {Atom.entryLinks = map (normalizeLink feedURL) entryLinks})
            feedEntries
      }
feedToAtom now feedURL feed = do
  feedUuid <- mkUuidUrn
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
  feedToAtom now feedURL . Feed.AtomFeed $
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
    normalizeLinkURL (URL feedUrl _) link
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
      uniqueEntries = nubOrdOn getItemLinkOrId allEntries
   in feed1 {Atom.feedEntries = sortBy (comparing (Down . Atom.entryUpdated)) uniqueEntries}

selectEntries :: (MonadIO m) => FeedTask -> UTCTime -> [Atom.Entry] -> [Atom.Entry] -> m [Atom.Entry]
selectEntries task now entries newEntries = do
  let newEntryLinks = HS.fromList $ map getItemLinkOrId newEntries

  fmap (filter (not . (`HS.member` newEntryLinks) . getItemLinkOrId))
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

computeNewEntries :: Bool -> Maybe Atom.Feed -> Either a Atom.Feed -> [Atom.Entry]
computeNewEntries passthroughNewEntries mSourceFeed eCachedFeed
  | passthroughNewEntries,
    Just sourceFeed <- mSourceFeed,
    Right cachedFeed <- eCachedFeed =
      let cachedFeedLinks = HS.fromList $ map getItemLinkOrId $ Atom.feedEntries cachedFeed
       in filter (not . (`HS.member` cachedFeedLinks) . getItemLinkOrId) $ Atom.feedEntries sourceFeed
  | otherwise = []

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
