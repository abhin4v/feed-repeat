module Main where

import Control.Arrow ((>>>))
import Control.Exception (IOException, displayException, try)
import Control.Monad (forM_, when, (>=>))
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Either (isRight)
import Data.Either.Extra (fromEither)
import Data.Foldable (traverse_)
import Data.Hashable (hash)
import Data.List (nub, (\\))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time (NominalDiffTime, TimeZone, UTCTime (..))
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml qualified as Yaml
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

timerTolerance :: NominalDiffTime
timerTolerance = 5 * 60 -- 5 minutes tolerance for systemd timer imprecision

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
  runningUnderSystemd <- (== Just "1") <$> lookupEnv "RUNNING_UNDER_SYSTEMD"
  tz <- Time.getCurrentTimeZone
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
    else return $ Just tasks

runTask :: FeedTask -> App ()
runTask task = do
  env <- ask
  let url = task.sourceFeedUrl
  logMsg DBG $ "Processing: " <> show url
  logMsg DBG $
    ("Task params: saveSourceFeedEntries=" <> show task.saveSourceFeedEntries)
      <> (", repeatedEntryCount=" <> show task.repeatedEntryCount)
      <> (", minimumEntryAgeDays=" <> show task.minimumEntryAgeDays)
      <> (", minRunGapDays=" <> show task.minRunGapDays)
  now <- liftIO Time.getCurrentTime
  let outputPath = env.options.outputDir </> task.outputFilename <> ".atom"

  outputFeed <-
    (Just <$> parseAtomFile outputPath) `catchError` \err -> do
      logMsg WRN $ "Failed to read output feed: " <> show err
      return Nothing
  let ancientTime = UTCTime (Time.fromGregorian 2000 1 1) 0
      outputFeedUpdated = case outputFeed of
        Nothing -> ancientTime
        Just outputFeed -> fromMaybe ancientTime $ parseDate $ Atom.feedUpdated outputFeed
  let minRunGapSeconds = fromIntegral task.minRunGapDays.toNum * Time.nominalDay - timerTolerance
  if Time.diffUTCTime now outputFeedUpdated < minRunGapSeconds
    then logMsg INF $ "Skipping run for URL: " <> show url
    else
      fetchCacheFeed task.saveSourceFeedEntries task.sourceFeedUrl outputFeedUpdated
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
  now <- liftIO Time.getCurrentTime
  let timestamp = T.pack $ iso8601Show now
  selectedEntries <-
    selectEntries task allEntries
      >>= traverse
        ( \e -> do
            entryId <- mkUuidUrn
            return e {Atom.entryId = entryId, Atom.entryUpdated = timestamp}
        )
  if null selectedEntries
    then logMsg WRN "Selected no entries for repetition"
    else do
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
      let url = task.sourceFeedUrl
      content <-
        fromMaybeOrThrow (FeedRenderError url.toString) . Feed.textFeed $ Feed.AtomFeed resultFeed
      env <- ask
      let outputPath = env.options.outputDir </> task.outputFilename <> ".atom"
      writeFile outputPath content
      tryOrThrow IOError $
        setFileMode outputPath $
          foldr1 unionFileModes [ownerReadMode, ownerWriteMode, groupReadMode]
      logMsg DBG $ "Wrote to: " <> outputPath
      logMsg INF $ "Processed " <> show url <> " successfully"

fetchCacheFeed :: Bool -> URL -> UTCTime -> App Atom.Feed
fetchCacheFeed saveSourceFeedEntries url feedUpdated = do
  env <- ask
  let cacheFilePath = env.options.cacheDir </> show (hash url) <> ".atom"
  freshOrCachedFeed <-
    (Right <$> fetchFeed url feedUpdated) `catchError` \err -> do
      case err of
        FeedNotModifiedError -> logMsg DBG $ "Feed not modified: " <> show url <> ", using cached"
        _ ->
          logMsg WRN $
            "Unable to fetch fresh feed: " <> show url <> ", using cached: " <> show err
      Left <$> parseAtomFile cacheFilePath
  mergedFeed <-
    if saveSourceFeedEntries
      then case freshOrCachedFeed of
        Right freshFeed ->
          (mergeFeeds freshFeed <$> parseAtomFile cacheFilePath) `catchError` \_ -> return freshFeed
        Left cachedFeed -> return cachedFeed
      else return $ fromEither freshOrCachedFeed

  when (isRight freshOrCachedFeed) $
    case Feed.textFeed (Feed.AtomFeed mergedFeed) of
      Nothing -> logMsg WRN $ "Failed to export feed for URL: " <> show url
      Just txt ->
        (writeFile cacheFilePath txt >> logMsg DBG ("Cached to: " <> cacheFilePath))
          `catchError` (logMsg WRN . ("Failed to write cache file: " <>) . show)

  return mergedFeed

fetchFeed :: URL -> UTCTime -> App Atom.Feed
fetchFeed url modTime = do
  atomFeed <- fetchAndParse url.toString
  logMsg DBG $
    "Fetched feed with " <> show (length $ Atom.feedEntries atomFeed) <> " entries"
  return atomFeed
  where
    fetchAndParse =
      HTTP.parseRequest
        >>> tryOrThrow HTTPError
        >=> addHeaders
        >>> HTTP.httpLBS
        >>> tryOrThrow HTTPError
        >=> checkForStatusNotModified
        >>> fromMaybeOrThrow FeedNotModifiedError
        >=> Feed.parseFeedSource
        >>> fromMaybeOrThrow (FeedParseError url.toString)
        >=> feedToAtom

    addHeaders request =
      request
        { HTTP.responseTimeout = HTTP.responseTimeoutMicro requestTimeoutMicros,
          HTTP.requestHeaders =
            HTTP.requestHeaders request
              <> [ (HTTP.hUserAgent, "feed-repeat"),
                   ( HTTP.hIfModifiedSince,
                     BSC.pack
                       . Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat
                       $ Time.utcToZonedTime (read "GMT") modTime
                   )
                 ]
        }

    checkForStatusNotModified resp
      | HTTP.responseStatus resp == HTTP.status304 = Nothing
      | otherwise = Just $ HTTP.responseBody resp

parseAtomFile :: FilePath -> App Atom.Feed
parseAtomFile filePath = do
  feed <- readAndParse filePath
  case feed of
    Feed.AtomFeed af -> do
      logMsg DBG $
        ("Parsed Atom file " <> filePath <> " with ")
          <> (show (length $ Feed.getFeedItems feed) <> " entries")
      return af
    _ -> throwError $ InvalidFormatError "Atom" filePath
  where
    readAndParse =
      readFile
        >>> tryOrThrow IOError
        >=> Feed.parseFeedString
        >>> fromMaybeOrThrow (FeedParseError filePath)

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
          now <- liftIO Time.getCurrentTime
          let localTime = Time.utcToLocalTime logConfig.timeZone now
          let timestamp = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
          return $ timestamp <> " "
  liftIO $ putStrLn logLine

writeFile :: FilePath -> TL.Text -> App ()
writeFile fp content = do
  let dir = takeDirectory fp
  tryOrThrow IOError $ withTempFile dir "feed-repeat-" $ \tmpFP tmpH -> do
    BS.hPutStr tmpH . TE.encodeUtf8 $ TL.toStrict content
    hClose tmpH
    renameFile tmpFP fp
