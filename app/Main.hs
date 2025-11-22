{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Strict #-}

module Main where

import Control.Applicative ((<|>))
import Control.Exception (Exception, IOException, displayException, try)
import Control.Monad (forM, forM_, join, mplus, when, (>=>))
import Control.Monad.Except (ExceptT, catchError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable, hash)
import Data.List (nubBy, sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime (..), diffUTCTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM, rfc822DateFormat)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID.V4 qualified as UUID
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Options.Applicative qualified as Opt
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Random (randomRIO)
import Text.Atom.Feed qualified as Atom
import Text.Feed.Export qualified as Feed
import Text.Feed.Import qualified as Feed
import Text.Feed.Query qualified as Feed
import Text.Feed.Types qualified as Feed
import Prelude hiding (writeFile)

-- | This module implements a feed repeater tool that processes RSS/Atom feeds.
--
-- It reads a YAML configuration file containing feed sources with settings for caching,
-- output filenames, and repetition parameters.
--
-- For each valid feed, it fetches the latest entries, filters out entries newer than the
-- minimum age threshold, and selects a random subset for repetition based on weighted
-- sampling where older entries have higher probability.
--
-- Selected entries are assigned new timestamps and UUIDs, and added to the output file.
data LogLevel = ERR | WRN | INF | DBG deriving (Show)

newtype URL = URL String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Hashable)

data FeedTask = FeedTask
  { sourceFeedUrl :: URL,
    outputFilename :: String,
    cacheSourceFeed :: Bool,
    repeatedEntryCount :: Int,
    minimumEntryAgeDays :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON)

data Options = Options
  { configPath :: FilePath,
    outputDir :: FilePath,
    cacheDir :: FilePath
  }

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

type App a = ExceptT AppError (ReaderT Options IO) a

minRunGapDays :: Int
minRunGapDays = 1

selectWeightDoublingDays :: Double
selectWeightDoublingDays = 7

requestTimeoutMicros :: Int
requestTimeoutMicros = 30_000_000 -- 30 sec

main :: IO ()
main = do
  options <-
    Opt.execParser $
      Opt.info
        optionsParser
        ( Opt.fullDesc
            <> Opt.progDesc "feed-repeat repeats entries of given feeds into new feeds"
            <> Opt.header "feed-repeat"
        )
  createDirs [options.outputDir, options.cacheDir]
  run options
  where
    createDirs = traverse_ $ \dir ->
      try (createDirectoryIfMissing True dir) >>= \case
        Left (e :: IOException) ->
          logMsg ERR ("Failed to create directory: " <> displayException e) >> exitFailure
        Right _ -> return ()

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.strOption
      ( Opt.long "config"
          <> Opt.metavar "FILE"
          <> Opt.help "Path to YAML config file containing feed sources"
      )
    <*> Opt.strOption
      ( Opt.long "output-dir"
          <> Opt.metavar "DIR"
          <> Opt.help "Directory where output Atom files will be written"
      )
    <*> Opt.strOption
      ( Opt.long "cache-dir"
          <> Opt.metavar "DIR"
          <> Opt.value "."
          <> Opt.help "Directory where cached Atom files will be stored"
      )
      Opt.<**> Opt.helper

run :: Options -> IO ()
run options =
  Yaml.decodeFileEither options.configPath >>= \case
    Left err -> do
      logMsg ERR $ "Error reading config: " <> show err
      exitFailure
    Right tasks | null tasks -> logMsg ERR "No tasks found in file" >> exitFailure
    Right tasks -> do
      validationResults <- forM tasks $ \task -> do
        let URL url = task.sourceFeedUrl
        res <- try @HTTP.HttpException $ HTTP.parseRequest url
        return (task, isRight res)
      let validTasks = map fst $ filter snd validationResults
      let invalidTasks = map fst $ filter (not . snd) validationResults
      if not (null invalidTasks)
        then do
          logMsg ERR "Invalid source feed URLs in tasks:"
          forM_ invalidTasks $ \task ->
            let URL url = task.sourceFeedUrl in logMsg ERR $ "  " <> url
          exitFailure
        else forM_ validTasks $ \task ->
          runReaderT (runExceptT $ runTask task) options >>= \case
            Left err -> logMsg ERR $ show err
            Right _ -> return ()

runTask :: FeedTask -> App ()
runTask task = do
  options <- ask
  let URL url = task.sourceFeedUrl
  logMsg DBG $ "Processing: " <> url
  now <- liftIO getCurrentTime
  let outputPath = options.outputDir </> task.outputFilename <> ".atom"

  outputFeed <-
    (Just <$> parseAtomFile outputPath) `catchError` \err -> do
      logMsg WRN $ "Failed to read output feed: " <> show err
      return Nothing
  let outputFeedUpdated = case outputFeed of
        Nothing -> UTCTime (fromGregorian 2000 1 1) 0
        Just outputFeed -> fromMaybe now $ parseDate $ Atom.feedUpdated outputFeed
  if diffUTCTime now outputFeedUpdated < fromIntegral minRunGapDays * 86400
    then logMsg INF $ "Skipping run for URL: " <> url
    else do
      fetchCacheFeed task.cacheSourceFeed task.sourceFeedUrl
        >>= processSourceFeed task outputFeed

processSourceFeed :: FeedTask -> Maybe Atom.Feed -> Atom.Feed -> App ()
processSourceFeed task mOutputFeed sourceFeed = do
  -- merge source and output feeds
  mergedFeed <- case mOutputFeed of
    Nothing -> return sourceFeed
    Just outputFeed -> return $ mergeFeeds sourceFeed outputFeed

  let allEntries = Atom.feedEntries mergedFeed
  logMsg DBG $ "Merged feed has " <> show (length allEntries) <> " entries"

  -- select entries
  now <- liftIO getCurrentTime
  let timestamp = T.pack $ iso8601Show now
      minAgeSeconds = fromIntegral task.minimumEntryAgeDays * 86400
  selectedEntries <-
    liftIO (selectEntries task.repeatedEntryCount minAgeSeconds allEntries)
      >>= traverse
        ( \e -> do
            entryId <- mkUuidUrn
            return e {Atom.entryId = entryId, Atom.entryUpdated = timestamp}
        )
  logMsg DBG $ "Selected " <> show (length selectedEntries) <> " entries for repetition"

  -- merge entries
  let outputFeedEntries = maybe [] Atom.feedEntries mOutputFeed
  let combinedEntries = selectedEntries <> outputFeedEntries
  logMsg DBG $
    "Combined entries: "
      <> (show (length selectedEntries) <> " new + ")
      <> (show (length outputFeedEntries) <> " existing = ")
      <> show (length combinedEntries)

  -- create new output feed
  let resultFeed =
        (fromMaybe sourceFeed mOutputFeed)
          { Atom.feedUpdated = T.pack $ iso8601Show now,
            Atom.feedEntries = combinedEntries
          }

  -- write new output feed
  let URL url = task.sourceFeedUrl
  content <-
    fromMaybeOrThrow (FeedRenderError url) . Feed.textFeed $ Feed.AtomFeed resultFeed
  options <- ask
  let outputPath = options.outputDir </> task.outputFilename <> ".atom"
  writeFile outputPath content
  logMsg DBG $ "Wrote to: " <> outputPath
  logMsg INF $ "Processed " <> url <> " successfully"

fetchCacheFeed :: Bool -> URL -> App Atom.Feed
fetchCacheFeed cache (URL url) = do
  options <- ask
  let filePath = options.cacheDir </> show (hash url) <> ".atom"
  freshOrCachedFeed <-
    (Right <$> fetchFeed url) `catchError` \err ->
      if cache
        then do
          logMsg WRN $
            "Unable to fetch fresh feed for URL: " <> url <> ", using cached: " <> show err
          Left <$> parseAtomFile filePath
        else throwError err
  mergedFeed <-
    if cache
      then case freshOrCachedFeed of
        Right freshFeed ->
          (mergeFeeds freshFeed <$> parseAtomFile filePath) `catchError` \_ -> return freshFeed
        Left cachedFeed -> return cachedFeed
      else return $ rightOrLeft freshOrCachedFeed

  when (cache && isRight freshOrCachedFeed) $
    case Feed.textFeed (Feed.AtomFeed mergedFeed) of
      Nothing -> logMsg WRN $ "Failed to export feed for URL: " <> url
      Just txt ->
        (writeFile filePath txt >> logMsg DBG ("Cached to: " <> filePath))
          `catchError` (logMsg WRN . ("Failed to write cache file: " <>) . show)

  return mergedFeed
  where
    rightOrLeft = \case Right a -> a; Left a -> a

fetchFeed :: String -> App Atom.Feed
fetchFeed url = do
  atomFeed <-
    tryOrThrow HTTPError (HTTP.parseRequest url)
      >>= tryOrThrow HTTPError . HTTP.httpLBS . addHeaders
      >>= fromMaybeOrThrow (FeedParseError url) . Feed.parseFeedSource . HTTP.getResponseBody
      >>= feedToAtom
  logMsg DBG $
    "Fetched feed with " <> show (length $ Atom.feedEntries atomFeed) <> " entries"
  return atomFeed
  where
    addHeaders request =
      request
        { HTTP.responseTimeout = HTTP.responseTimeoutMicro requestTimeoutMicros,
          HTTP.requestHeaders =
            HTTP.requestHeaders request <> [(HTTP.hUserAgent, "feed-repeat")]
        }

feedToAtom :: Feed.Feed -> App Atom.Feed
feedToAtom (Feed.AtomFeed af) = return af
feedToAtom feed = do
  feedUuid <- mkUuidUrn
  now <- liftIO getCurrentTime
  entries <-
    sortBy (comparing (Down . Atom.entryUpdated))
      <$> traverse (itemToAtomEntry now) (Feed.getFeedItems feed)
  let title = Feed.getFeedTitle feed
      link = Feed.getFeedHome feed
      updateDate = Feed.getFeedLastUpdate feed
      pubDate = Feed.getFeedPubDate feed
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
        Just entryTime -> diffUTCTime currentTime entryTime >= fromInteger minAgeSeconds

    computeWeight :: UTCTime -> Atom.Entry -> Double
    computeWeight now entry = case Feed.getItemPublishDate (Feed.AtomItem entry) of
      Nothing -> 1
      Just Nothing -> 1
      Just (Just updated) ->
        let age = diffUTCTime now updated
         in if age > 0 then exp (realToFrac age / (86400 * selectWeightDoublingDays)) else 1

    select now es = do
      keys <- forM es $ \entry -> do
        r <- randomRIO (0, 1)
        return $ r ** (1 / computeWeight now entry)
      return $ take n $ map fst $ sortBy (comparing (Down . snd)) $ zip es keys

parseAtomFile :: FilePath -> App Atom.Feed
parseAtomFile filePath = do
  feed <-
    tryOrThrow IOError (readFile filePath)
      >>= fromMaybeOrThrow (FeedParseError filePath) . Feed.parseFeedString
  case feed of
    Feed.AtomFeed af -> do
      logMsg DBG $
        ("Parsed Atom file " <> filePath <> " with ")
          <> (show (length $ Feed.getFeedItems feed) <> " entries")
      return af
    _ -> throwError $ InvalidFormatError "Atom" filePath

logMsg :: (MonadIO m) => LogLevel -> String -> m ()
logMsg level msg = do
  now <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime tz now
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
  liftIO $ putStrLn $ timestamp <> " [" <> show level <> "] " <> msg

writeFile :: FilePath -> TL.Text -> App ()
writeFile fp content = do
  let tmpFP = fp <> ".tmp"
  tryOrThrow IOError $ BS.writeFile tmpFP . TE.encodeUtf8 $ TL.toStrict content
  tryOrThrow IOError $ renameFile tmpFP fp

mkUuidUrn :: (MonadIO m) => m T.Text
mkUuidUrn = T.pack . ("urn:uuid:" <>) . show <$> liftIO UUID.nextRandom

parseDate :: T.Text -> Maybe UTCTime
parseDate ds = do
  let rfc3339DateFormat1 = "%Y-%m-%dT%H:%M:%S%Z"
      rfc3339DateFormat2 = "%Y-%m-%dT%H:%M:%S%Q%Z"
      formats = [rfc3339DateFormat1, rfc3339DateFormat2, rfc822DateFormat]
  foldl1 mplus (map (\fmt -> parseTimeM True defaultTimeLocale fmt $ T.unpack ds) formats)

tryOrThrow :: (Exception e) => (e -> AppError) -> IO b -> App b
tryOrThrow mkErr = liftIO . try >=> liftEither . mapLeft mkErr

fromMaybeOrThrow :: AppError -> Maybe a -> App a
fromMaybeOrThrow err = liftEither . maybeToRight err
