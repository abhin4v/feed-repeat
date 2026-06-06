module Main where

import Control.Arrow ((>>>))
import Control.Exception (IOException, displayException, throwIO, toException, try)
import Control.Monad (forM, forM_, unless, void, when, (>=>))
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Logger
  ( LogLevel (..),
    LoggingT,
    MonadLogger,
    filterLogger,
    logDebugN,
    logErrorN,
    logInfoN,
    logWarnN,
    runStdoutLoggingT,
  )
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Retry qualified as Retry
import Crypto.Hash (Digest, SHA256)
import Crypto.Hash qualified as Crypto
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isRight)
import Data.Either.Extra (fromEither)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, nub, zip4, (\\))
import Data.List.Extra (nubOrd)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time (NominalDiffTime, UTCTime (..))
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Version (showVersion)
import Data.Yaml qualified as Yaml
import Lib
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Options.Applicative qualified as Opt
import PackageInfo_feed_repeat qualified as PI
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Exit (exitFailure)
import System.FilePath (splitDirectories, takeDirectory, (<.>), (</>))
import System.IO (hClose)
import System.IO.Error (illegalOperationErrorType, mkIOError)
import System.IO.Temp (withTempFile)
import System.Posix.Files
  ( groupReadMode,
    ownerReadMode,
    ownerWriteMode,
    setFileMode,
    unionFileModes,
  )
import System.Posix.Types (FileMode)
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
data Options = Options
  { configPath :: FilePath,
    outputDir :: FilePath,
    cacheDir :: FilePath,
    validateOnly :: Bool,
    verbose :: Bool,
    quiet :: Bool
  }

data Env = Env {options :: Options, httpManager :: HTTP.Manager, startTime :: UTCTime}

type App a = ExceptT AppError (ReaderT Env (LoggingT IO)) a

requestTimeoutMicros :: Int
requestTimeoutMicros = 30_000_000 -- 30 sec

timerTolerance :: NominalDiffTime
timerTolerance = 5 * 60 -- 5 minutes tolerance for systemd timer imprecision

maxBodySize :: Int
maxBodySize = 10 * 1024 * 1024 -- 10 MB

fileMode :: FileMode
fileMode = foldr1 unionFileModes [ownerReadMode, ownerWriteMode, groupReadMode]

ancientTime :: UTCTime
ancientTime = UTCTime (Time.fromGregorian 2000 1 1) 0

main :: IO ()
main = do
  now <- Time.getCurrentTime
  options' <-
    Opt.execParser $
      Opt.info
        ( optionsParser
            Opt.<**> Opt.helper
            Opt.<**> Opt.simpleVersioner ("feed-repeat version " <> showVersion PI.version <> " © " <> PI.copyright)
        )
        ( Opt.fullDesc
            <> Opt.progDesc PI.synopsis
            <> Opt.header ("feed-repeat version " <> showVersion PI.version)
            <> Opt.footer (PI.homepage <> " © " <> PI.copyright)
        )
  let manSettings =
        HTTP.tlsManagerSettings
          { HTTP.managerModifyRequest = \request -> do
              let url = show $ HTTP.getUri request
              checkPublicUrl url >>= \case
                True -> return request
                False -> throwIO $ HTTP.InvalidUrlException url "Request to private URL"
          }
  man <- HTTP.newTlsManagerWith manSettings

  outputDir <- canonicalizePath options'.outputDir
  cacheDir <- canonicalizePath options'.cacheDir
  let options = options' {outputDir, cacheDir}

  let env = Env options man now
  createDirs [options.outputDir, options.cacheDir]
  run env
  where
    createDirs = traverse_ $ \dir ->
      try (createDirectoryIfMissing True dir) >>= \case
        Left (e :: IOException) -> do
          logErrorIO $ "Failed to create directory: " <> displayException e
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
    <*> Opt.switch
      (Opt.long "validate" <> Opt.help "Only validate the config file and exit")
    <*> Opt.switch
      (Opt.long "verbose" <> Opt.help "Enable all logging")
    <*> Opt.switch
      (Opt.long "quiet" <> Opt.help "Enable only warning and error logging")

run :: Env -> IO ()
run env =
  Yaml.decodeFileEither env.options.configPath >>= \case
    Left err -> logErrorIO ("Error reading config: " <> show err) >> exitFailure
    Right tasks | null tasks -> logErrorIO "No tasks found in file" >> exitFailure
    Right tasks ->
      validateTasks env.options.outputDir tasks >>= \case
        Nothing -> exitFailure
        Just validated
          | env.options.validateOnly ->
              unless env.options.quiet $ do
                logInfoIO $ "Config valid: " <> show (length validated) <> " tasks"
          | otherwise -> do
              migrateCacheFile env.options.cacheDir validated
              runStdoutLoggingT
                . filterLogger enableLogging
                . flip runReaderT env
                $ runTasks validated
  where
    runTasks tasks = forM_ (nubOrd $ map sourceFeedUrl tasks) $ \url ->
      runExceptT (runTasksForSource (filter ((== url) . sourceFeedUrl) tasks) url) >>= \case
        Left err -> logError $ show err
        Right _ -> return ()

    enableLogging _ level
      | env.options.quiet = level >= LevelWarn
      | env.options.verbose = True
      | otherwise = level >= LevelInfo

migrateCacheFile :: FilePath -> [FeedTask] -> IO ()
migrateCacheFile cacheDir tasks = do
  let sourceFeedUrls = nubOrd $ map sourceFeedUrl tasks
  forM_ sourceFeedUrls $ \url -> do
    let oldFileName = cacheDir </> oldCacheFileName url
        tasksWithSource = [t | t <- tasks, t.sourceFeedUrl == url]
    doesFileExist oldFileName >>= \case
      False -> return ()
      True -> do
        results <- forM tasksWithSource $ \task -> do
          let newFileName = cacheDir </> cacheFileName task.sourceFeedUrl task.outputFilename
          newFileExists <- doesFileExist newFileName
          if newFileExists
            then return True
            else do
              try (copyFile oldFileName newFileName) >>= \case
                Left (e :: IOException) -> do
                  logWarnIO $ "Cache migration failed for " <> oldFileName <> ": " <> displayException e
                  return False
                Right _ -> do
                  logInfoIO $ "Migrated cache file: " <> oldFileName <> " to " <> newFileName
                  return True
        when (and results) $
          try (removeFile oldFileName) >>= \case
            Right () -> return ()
            Left (e :: IOException) -> do
              logWarnIO $ "Failed to remove old cache file " <> oldFileName <> ": " <> displayException e

validateTasks :: FilePath -> [FeedTask] -> IO (Maybe [FeedTask])
validateTasks outputDir tasks = do
  tasks <- fmap catMaybes . forM tasks $ \task ->
    checkPublicUrl task.sourceFeedUrl.toString >>= \case
      True -> do
        outputFP <- canonicalizePath $ outputDir </> task.outputFilename <.> "atom"
        if splitDirectories outputDir `isPrefixOf` splitDirectories outputFP
          then return $ Just task
          else do
            logErrorIO $ "Output file is outside output directory: " <> outputFP
            return Nothing
      False -> do
        logErrorIO $ "Private source feed URL found: " <> task.sourceFeedUrl.toString
        return Nothing

  -- Check for duplicate output filenames
  let outputFilenames = map outputFilename tasks
  let duplicates = outputFilenames \\ nub outputFilenames
  if not (null duplicates)
    then do
      logErrorIO "Duplicate output filenames found:"
      forM_ (nub duplicates) (logErrorIO . ("  " <>))
      return Nothing
    else return $ if null tasks then Nothing else Just tasks

runTasksForSource :: [FeedTask] -> URL -> App ()
runTasksForSource tasks sourceFeedUrl = do
  env <- ask
  -- parse output files
  (tasks, mOutputFeeds) <- unzip . catMaybes <$> traverse mParseOutputFile tasks

  -- get output file updates times
  let (outputFeedsUpdatedAncient, outputFeedsUpdatedNow) = unzip $ flip map mOutputFeeds $ \case
        Nothing -> (ancientTime, env.startTime)
        Just outputFeed ->
          let updated = parseDate $ Atom.feedUpdated outputFeed
           in (fromMaybe ancientTime updated, fromMaybe env.startTime updated)

  -- skip run or get run task for tasks
  actions <- fmap catMaybes . forM (zip4 tasks mOutputFeeds outputFeedsUpdatedAncient outputFeedsUpdatedNow) $
    \(task, mOutputFeed, outputFeedUpdatedAncient, outputFeedUpdatedNow) -> do
      let minRunGapSeconds = fromIntegral task.minRunGapDays.toNum * Time.nominalDay - timerTolerance
      if Time.diffUTCTime env.startTime outputFeedUpdatedAncient < minRunGapSeconds
        then do
          logInfo $ "Skipping run for URL: " <> show sourceFeedUrl
          return Nothing
        else return $ Just $ \mSourceFeed ->
          runTask task mSourceFeed mOutputFeed outputFeedUpdatedNow `catchError` (logError . show)

  -- run tasks
  unless (null actions) $ do
    mSourceFeed <- mFetchFeed sourceFeedUrl $ maximum outputFeedsUpdatedAncient
    traverse_ ($ mSourceFeed) actions
  where
    mFetchFeed url modTime =
      (Just <$> fetchFeed url modTime) `catchError` \err -> do
        case err of
          FeedNotModifiedError -> logDebug $ "Feed not modified: " <> show url <> ", using cached"
          _ -> logWarn $ "Unable to fetch fresh feed: " <> show url <> ", using cached: " <> show err
        return Nothing

    mParseOutputFile task = do
      env <- ask
      let outputPath = env.options.outputDir </> task.outputFilename <> ".atom"

      (Just . (task,) . Just <$> parseAtomFile outputPath) -- file parses, task runs
        `catchError` \case
          IOError err -> do
            -- file missing or unreadable, task still runs
            logWarn $ "Failed to read output feed: " <> show err
            return $ Just (task, Nothing)
          err -> do
            -- file corrupted, tasks does not run
            logError $ "Corrupted output feed: " <> show err
            return Nothing

    runTask task sourceFeed outputFeed outputFeedUpdated = do
      let url = task.sourceFeedUrl
      logDebug $ "Processing: " <> show url
      logDebug $
        ("Task params: saveSourceFeedEntries=" <> show task.saveSourceFeedEntries)
          <> (", repeatedEntryCount=" <> show task.repeatedEntryCount)
          <> (", minimumEntryAgeDays=" <> show task.minimumEntryAgeDays)
          <> (", minRunGapDays=" <> show task.minRunGapDays)

      cacheSourceFeed task sourceFeed
        >>= processSourceFeed task outputFeed outputFeedUpdated

processSourceFeed :: FeedTask -> Maybe Atom.Feed -> UTCTime -> Atom.Feed -> App ()
processSourceFeed task mOutputFeed outputFeedUpdated sourceFeed = do
  -- merge source and output feeds
  mergedFeed <- case mOutputFeed of
    Nothing -> return sourceFeed
    Just outputFeed -> return $ mergeFeeds sourceFeed outputFeed

  let allEntries = Atom.feedEntries mergedFeed
  logDebug $ "Merged feed has " <> show (length allEntries) <> " entries"

  -- select entries
  env <- ask
  let timestamp = T.pack $ iso8601Show env.startTime
  (selectedEntries', newEntries') <- selectEntries task outputFeedUpdated env.startTime allEntries
  [selectedEntries, newEntries] <- traverse (traverse (resetEntryId timestamp)) [selectedEntries', newEntries']

  if null selectedEntries && null newEntries
    then logWarn "Selected no new entries or entries for repetition"
    else do
      logDebug $ "Selected " <> show (length selectedEntries) <> " entries for repetition"
      logDebug $ "Got " <> show (length newEntries) <> " new entries"

      -- merge entries
      let outputFeedEntries = maybe [] Atom.feedEntries mOutputFeed
      let combinedEntries = newEntries <> selectedEntries <> outputFeedEntries
      logDebug $
        "Combined entries: "
          <> (show (length newEntries) <> " new + ")
          <> (show (length selectedEntries) <> " repeated + ")
          <> (show (length outputFeedEntries) <> " existing = ")
          <> show (length combinedEntries)

      -- create new output feed
      let resultFeed =
            (fromMaybe sourceFeed mOutputFeed)
              { Atom.feedUpdated = timestamp,
                Atom.feedEntries = combinedEntries
              }

      -- write new output feed
      let url = task.sourceFeedUrl
      content <-
        fromMaybeOrThrow (FeedRenderError url.toString) . Feed.textFeed $ Feed.AtomFeed resultFeed
      let outputPath = env.options.outputDir </> task.outputFilename <> ".atom"
      writeFile outputPath content
      logDebug $ "Wrote to: " <> outputPath
      logInfo $ "Processed " <> show url <> " successfully"
  where
    resetEntryId timestamp entry = do
      entryId <- mkUuidUrn
      return entry {Atom.entryId = entryId, Atom.entryUpdated = timestamp}

oldCacheFileName :: URL -> String
oldCacheFileName url =
  let d :: Digest SHA256 = Crypto.hash $ TE.encodeUtf8 $ T.pack $ url.toString
   in show d <> ".atom"

cacheFileName :: URL -> String -> String
cacheFileName url outputFilename =
  let d :: Digest SHA256 = Crypto.hash $ TE.encodeUtf8 $ T.pack $ url.toString <> ['\0'] <> outputFilename
   in show d <> ".atom"

cacheSourceFeed :: FeedTask -> Maybe Atom.Feed -> App Atom.Feed
cacheSourceFeed task mFeed = do
  env <- ask
  let url = task.sourceFeedUrl
  let cacheFilePath = env.options.cacheDir </> cacheFileName url task.outputFilename
  freshOrCachedFeed <- case mFeed of
    Just feed -> pure $ Right feed
    Nothing -> Left <$> parseAtomFile cacheFilePath
  mergedFeed <-
    if task.saveSourceFeedEntries
      then case freshOrCachedFeed of
        Right freshFeed ->
          (mergeFeeds freshFeed <$> parseAtomFile cacheFilePath) `catchError` \err -> do
            logWarn $ "Unable to parse cache file " <> cacheFilePath <> ", overwriting with source feed: " <> show err
            return freshFeed
        Left cachedFeed -> return cachedFeed
      else return $ fromEither freshOrCachedFeed

  when (isRight freshOrCachedFeed) $
    case Feed.textFeed (Feed.AtomFeed mergedFeed) of
      Nothing -> logWarn $ "Failed to export feed for URL: " <> show url
      Just txt ->
        (writeFile cacheFilePath txt >> logDebug ("Cached to: " <> cacheFilePath))
          `catchError` (logWarn . ("Failed to write cache file: " <>) . show)

  return mergedFeed

fetchFeed :: URL -> UTCTime -> App Atom.Feed
fetchFeed url modTime = do
  env <- ask
  feed <- fetchAndParse env.startTime env.httpManager url.toString
  logDebug $ "Fetched feed with " <> show (length $ Atom.feedEntries feed) <> " entries: " <> url.toString
  return feed
  where
    fetchAndParse now man =
      HTTP.parseRequest
        >>> tryOrThrow HTTPError
        >=> addHeaders
        >>> fetchWithRetry man
        >>> tryOrThrow HTTPError
        >=> checkForStatusNotModified
        >>> fromMaybeOrThrow FeedNotModifiedError
        >=> Feed.parseFeedSource
        >>> fromMaybeOrThrow (FeedParseError url.toString)
        >=> feedToAtom now url

    addHeaders request =
      request
        { HTTP.responseTimeout = HTTP.responseTimeoutMicro requestTimeoutMicros,
          HTTP.requestHeaders =
            HTTP.requestHeaders request
              <> [ (HTTP.hUserAgent, "feed-repeat"),
                   ( HTTP.hIfModifiedSince,
                     BSC.pack
                       . Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat
                       $ Time.utcToZonedTime (Time.TimeZone 0 False "GMT") modTime
                   )
                 ]
        }

    fetchWithRetry man req =
      ( Retry.retrying
          (Retry.capDelay 60_000_000 $ Retry.exponentialBackoff 1_000_000 <> Retry.limitRetries 3)
          (const $ pure . HTTP.statusIsServerError . HTTP.responseStatus)
          . const
          $ httpLBS man req
      )
        >>= throwHttpErrors req

    throwHttpErrors req resp = do
      let status = HTTP.responseStatus resp
      unless (HTTP.statusIsSuccessful status || HTTP.statusIsRedirection status) $ do
        let chunk = LBS.take 1024 $ HTTP.responseBody resp
        let resp' = void resp
        let ex = HTTP.StatusCodeException resp' $ LBS.toStrict chunk
        throwIO $ HTTP.HttpExceptionRequest req ex

      return resp

    httpLBS man req = do
      HTTP.withResponse req man $ \res -> do
        bss <- consumeRespBodyWithLimit req $ HTTP.responseBody res
        return res {HTTP.responseBody = LBS.fromChunks bss}

    consumeRespBodyWithLimit req brRead = go 0 id
      where
        go size front = do
          x <- brRead
          if BS.null x
            then return $ front []
            else do
              let size' = size + BS.length x
              when (size' > maxBodySize)
                $ throwIO
                  . HTTP.HttpExceptionRequest req
                  . HTTP.InternalException
                  . toException
                $ mkIOError
                  illegalOperationErrorType
                  ("Feed body exceeded " <> show maxBodySize <> " bytes")
                  Nothing
                  Nothing

              go size' (front . (x :))

    checkForStatusNotModified resp
      | HTTP.responseStatus resp == HTTP.status304 = Nothing
      | otherwise = Just $ HTTP.responseBody resp

parseAtomFile :: FilePath -> App Atom.Feed
parseAtomFile filePath = do
  feed <- readAndParse filePath
  case feed of
    Feed.AtomFeed af -> do
      logDebug $
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

logInfoIO, logWarnIO, logErrorIO :: String -> IO ()
logInfoIO = runStdoutLoggingT . logInfo
logWarnIO = runStdoutLoggingT . logWarn
logErrorIO = runStdoutLoggingT . logError

logDebug, logInfo, logWarn, logError :: (MonadLogger m) => String -> m ()
logDebug = logDebugN . T.pack
logInfo = logInfoN . T.pack
logWarn = logWarnN . T.pack
logError = logErrorN . T.pack

writeFile :: FilePath -> TL.Text -> App ()
writeFile fp content = do
  let dir = takeDirectory fp
  tryOrThrow IOError $ withTempFile dir "feed-repeat-" $ \tmpFP tmpH -> do
    BS.hPutStr tmpH . TE.encodeUtf8 $ TL.toStrict content
    hClose tmpH
    renameFile tmpFP fp
    setFileMode fp fileMode
