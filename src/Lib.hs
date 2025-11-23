module Lib where

import Control.Applicative ((<|>))
import Control.Exception (Exception, IOException, displayException, try)
import Control.Monad (forM, join, mplus, (>=>))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.List (nubBy, sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Data.Time (UTCTime (..), diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Client qualified as HTTP
import System.Random (randomRIO)
import Text.Atom.Feed qualified as Atom
import Text.Feed.Query qualified as Feed
import Text.Feed.Types qualified as Feed
import Prelude hiding (writeFile)

data AppError
  = IOError IOException
  | FeedParseError FilePath
  | FeedRenderError FilePath
  | InvalidFormatError String FilePath
  | InvalidFeedUpdatedError
  | HTTPError HTTP.HttpException

instance Show AppError where
  show = \case
    IOError err -> "Failed to read/write file " <> displayException err
    FeedParseError filePath -> "Failed to parse: " <> filePath
    FeedRenderError filePath -> "Failed to render: " <> filePath
    InvalidFormatError format filePath -> "File is not in " <> format <> " format: " <> filePath
    InvalidFeedUpdatedError -> "Feed updated date absent"
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
            pubDate = join (Feed.getItemPublishDate @UTCTime item)
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
      uniqueEntries =
        nubBy
          (\a b -> Feed.getItemLink (Feed.AtomItem a) == Feed.getItemLink (Feed.AtomItem b))
          sortedEntries
   in feed1 {Atom.feedEntries = uniqueEntries}

selectEntries :: Int -> Integer -> [Atom.Entry] -> IO [Atom.Entry]
selectEntries n minAgeSeconds entries = do
  now <- getCurrentTime
  select now $ filter (isOldEnough now) entries
  where
    isOldEnough currentTime entry =
      case parseDate $ Atom.entryUpdated entry of
        Nothing -> True
        Just entryTime ->
          diffUTCTime currentTime entryTime >= fromInteger minAgeSeconds

    computeWeight :: UTCTime -> Atom.Entry -> Double
    computeWeight now entry = case parseDate $ Atom.entryUpdated entry of
      Nothing -> 1
      Just updated ->
        let age = diffUTCTime now updated
         in if age > 0 then exp (realToFrac age / (86400 * 365)) else 1

    -- A-Res algorithm
    select now es = do
      keys <- forM es $ \entry -> do
        r <- randomRIO (0, 1)
        return $ log r / computeWeight now entry
      return $ take n $ map fst $ sortBy (comparing (Down . snd)) $ zip es keys

mkUuidUrn :: (MonadIO m) => m T.Text
mkUuidUrn = T.pack . ("urn:uuid:" <>) . show <$> liftIO UUID.nextRandom

parseDate :: T.Text -> Maybe UTCTime
parseDate ds = do
  let rfc3339DateFormat1 = "%Y-%m-%dT%H:%M:%S%Z"
      rfc3339DateFormat2 = "%Y-%m-%dT%H:%M:%S%Q%Z"
      formats = [rfc3339DateFormat1, rfc3339DateFormat2, rfc822DateFormat]
  foldl1 mplus (map (\fmt -> parseTimeM True defaultTimeLocale fmt $ T.unpack ds) formats)

tryOrThrow :: (MonadIO m, Exception e1, MonadError e2 m) => (e1 -> e2) -> IO c -> m c
tryOrThrow mkErr = liftIO . try >=> liftEither . mapLeft mkErr

fromMaybeOrThrow :: (MonadError e m) => e -> Maybe a -> m a
fromMaybeOrThrow err = liftEither . maybeToRight err
