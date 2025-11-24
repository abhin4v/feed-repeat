{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Strict #-}

module Main where

import Control.Exception (IOException, displayException, try)
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (withObject, (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable, hash)
import Data.List (nub, (\\))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time
  ( TimeZone,
    UTCTime (..),
    diffUTCTime,
    getCurrentTime,
    getCurrentTimeZone,
    utcToLocalTime,
  )
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Lib
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Options.Applicative qualified as Opt
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.Posix.Files
  ( groupReadMode,
    ownerReadMode,
    ownerWriteMode,
    setFileMode,
    unionFileModes,
  )
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
    minimumEntryAgeDays :: Int,
    minRunGapDays :: Int
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON FeedTask where
  parseJSON = withObject "FeedTask" $ \v ->
    FeedTask
      <$> v .: "sourceFeedUrl"
      <*> v .: "outputFilename"
      <*> v .: "cacheSourceFeed"
      <*> v .: "repeatedEntryCount"
      <*> v .: "minimumEntryAgeDays"
      <*> v .:? "minRunGapDays" .!= 1

data LogConfig = LogConfig
  { omitTimestamp :: Bool,
    timeZone :: TimeZone
  }

data Options = Options
  { configPath :: FilePath,
    outputDir :: FilePath,
    cacheDir :: FilePath
  }

data Env = Env
  { options :: Options,
    logConfig :: LogConfig
  }

type App a = ExceptT AppError (ReaderT Env IO) a

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
  runningUnderSystemd <- fmap (== Just "1") $ lookupEnv "RUNNING_UNDER_SYSTEMD"
  tz <- getCurrentTimeZone
  let env = Env options $ LogConfig runningUnderSystemd tz
  createDirs env [options.outputDir, options.cacheDir]
  run env
  where
    createDirs env = traverse_ $ \dir ->
      try (createDirectoryIfMissing True dir) >>= \case
        Left (e :: IOException) -> do
          logIO env ERR $ "Failed to create directory: " <> displayException e
          exitFailure
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

run :: Env -> IO ()
run env =
  Yaml.decodeFileEither env.options.configPath >>= \case
    Left err -> logIO env ERR ("Error reading config: " <> show err) >> exitFailure
    Right tasks | null tasks -> logIO env ERR "No tasks found in file" >> exitFailure
    Right tasks ->
      validateTasks env tasks >>= \case
        Nothing -> exitFailure
        Just validated -> forM_ validated $ \task ->
          runReaderT (runExceptT $ runTask task) env >>= \case
            Left err -> logIO env ERR $ show err
            Right _ -> return ()

validateTasks :: Env -> [FeedTask] -> IO (Maybe [FeedTask])
validateTasks env tasks = do
  -- Check for duplicate output filenames
  let outputFilenames = map outputFilename tasks
  let duplicates = outputFilenames \\ nub outputFilenames
  if not (null duplicates)
    then do
      logIO env ERR "Duplicate output filenames found:"
      forM_ (nub duplicates) (logIO env ERR . ("  " <>))
      return Nothing
    else do
      -- Check for valid source feed URLs
      validationResults <- forM tasks $ \task -> do
        let URL url = task.sourceFeedUrl
        res <- try @HTTP.HttpException $ HTTP.parseRequest url
        return (task, isRight res)
      let invalidTasks = map fst $ filter (not . snd) validationResults
      if not (null invalidTasks)
        then do
          logIO env ERR "Invalid source feed URLs in tasks:"
          forM_ invalidTasks $ \task ->
            let URL url = task.sourceFeedUrl in logIO env ERR $ "  " <> url
          return Nothing
        else return $ Just tasks

runTask :: FeedTask -> App ()
runTask task = do
  env <- ask
  let URL url = task.sourceFeedUrl
  logMsg DBG $ "Processing: " <> url
  logMsg DBG $
    ("Task params: cacheSourceFeed=" <> show task.cacheSourceFeed)
      <> (", repeatedEntryCount=" <> show task.repeatedEntryCount)
      <> (", minimumEntryAgeDays=" <> show task.minimumEntryAgeDays)
      <> (", minRunGapDays=" <> show task.minRunGapDays)
  now <- liftIO getCurrentTime
  let outputPath = env.options.outputDir </> task.outputFilename <> ".atom"

  outputFeed <-
    (Just <$> parseAtomFile outputPath) `catchError` \err -> do
      logMsg WRN $ "Failed to read output feed: " <> show err
      return Nothing
  let outputFeedUpdated = case outputFeed of
        Nothing -> UTCTime (fromGregorian 2000 1 1) 0
        Just outputFeed -> fromMaybe now $ parseDate $ Atom.feedUpdated outputFeed
  if diffUTCTime now outputFeedUpdated < fromIntegral task.minRunGapDays * fromIntegral secondsPerDay
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
      minAgeSeconds = fromIntegral (task.minimumEntryAgeDays * fromIntegral secondsPerDay)
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
  env <- ask
  let outputPath = env.options.outputDir </> task.outputFilename <> ".atom"
  writeFile outputPath content
  tryOrThrow IOError $
    setFileMode outputPath $
      foldr1 unionFileModes [ownerReadMode, ownerWriteMode, groupReadMode]
  logMsg DBG $ "Wrote to: " <> outputPath
  logMsg INF $ "Processed " <> url <> " successfully"

fetchCacheFeed :: Bool -> URL -> App Atom.Feed
fetchCacheFeed cache (URL url) = do
  env <- ask
  let filePath = env.options.cacheDir </> show (hash url) <> ".atom"
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

logMsg :: LogLevel -> String -> App ()
logMsg level msg = do
  env <- ask
  logMsg' env.logConfig level msg

logIO :: Env -> LogLevel -> String -> IO ()
logIO env = logMsg' env.logConfig

logMsg' :: (MonadIO m) => LogConfig -> LogLevel -> String -> m ()
logMsg' logConfig level msg = do
  logLine <-
    (<> "[" <> show level <> "] " <> msg)
      <$> if logConfig.omitTimestamp
        then return ""
        else do
          now <- liftIO getCurrentTime
          let localTime = utcToLocalTime logConfig.timeZone now
          let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
          return $ timestamp <> " "
  liftIO $ putStrLn logLine

writeFile :: FilePath -> TL.Text -> App ()
writeFile fp content = do
  let dir = takeDirectory fp
  tryOrThrow IOError $ withTempFile dir "feed-repeat-" $ \tmpFP tmpH -> do
    BS.hPutStr tmpH . TE.encodeUtf8 $ TL.toStrict content
    hClose tmpH
    renameFile tmpFP fp
