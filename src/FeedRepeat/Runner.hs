module FeedRepeat.Runner
  ( Options (..),
    Env (..),
    runTasksForSource,
    cacheFileName,
    oldCacheFileName,
    logInfoIO,
    logWarnIO,
    logErrorIO,
    logDebug,
    logInfo,
    logWarn,
    logError,
  )
where

import Control.Arrow ((>>>))
import Control.Exception (throwIO, toException)
import Control.Monad (forM, unless, void, when, (>=>))
import Control.Monad.Except (ExceptT, catchError, throwError)
import Control.Monad.Logger
  ( LoggingT,
    MonadLogger,
    logDebugN,
    logErrorN,
    logInfoN,
    logWarnN,
    runStdoutLoggingT,
  )
import Control.Monad.Reader (ReaderT, ask)
import Control.Retry qualified as Retry
import Crypto.Hash (Digest, SHA256)
import Crypto.Hash qualified as Crypto
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isRight)
import Data.Either.Extra (fromEither)
import Data.Foldable (traverse_)
import Data.List (zip4)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time (NominalDiffTime, UTCTime (..))
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 (iso8601Show)
import FeedRepeat.Lib
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import System.Directory (renameFile)
import System.FilePath (takeDirectory, (</>))
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
          FeedTooManyRequestsError -> logDebug $ "Feed too many requests: " <> show url <> ", using cached"
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
      (HTTP.parseRequest >>> tryOrThrow HTTPError)
        >=> (addHeaders >>> fetchWithRetry man >>> tryOrThrow HTTPError)
        >=> (checkForStatus HTTP.status304 >>> fromMaybeOrThrow FeedNotModifiedError)
        >=> (checkForStatus HTTP.status429 >>> fromMaybeOrThrow FeedTooManyRequestsError)
        >=> (HTTP.responseBody >>> Feed.parseFeedSource >>> fromMaybeOrThrow (FeedParseError url.toString))
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
      unless (HTTP.statusIsSuccessful status || HTTP.statusIsRedirection status || status == HTTP.status429) $ do
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

    checkForStatus status resp
      | HTTP.responseStatus resp == status = Nothing
      | otherwise = Just resp

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
