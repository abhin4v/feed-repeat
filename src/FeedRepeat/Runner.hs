{-# LANGUAGE MultiWayIf #-}

module FeedRepeat.Runner
  ( runTasksForSource,
    cacheFileName,
    oldCacheFileName,
    logInfoIO,
    logErrorIO,
    migrateCacheFile,
    runAllTasks,
    checkDuplicateOutputs,
  )
where

import Control.Arrow ((>>>))
import Control.Exception (displayException, throwIO, toException)
import Control.Monad (forM, forM_, unless, void, when, (>=>))
import Control.Monad.Except (catchError, liftEither, throwError)
import Control.Monad.Logger
  ( MonadLogger,
    logDebugN,
    logErrorN,
    logInfoN,
    logWarnN,
    runStdoutLoggingT,
  )
import Control.Monad.Reader (ask)
import Control.Retry qualified as Retry
import Crypto.Hash (SHA256)
import Crypto.Hash qualified as Crypto
import Data.Aeson qualified as Aeson (eitherDecode', encode)
import Data.Bifunctor (second)
import Data.ByteString qualified as BS (length, null)
import Data.ByteString.Char8 qualified as BSC (pack)
import Data.ByteString.Lazy qualified as LBS (fromChunks, take, toStrict)
import Data.Either (isRight)
import Data.Either.Extra (mapLeft)
import Data.Foldable (traverse_)
import Data.List (nub, (\\))
import Data.List.Extra (nubOrd)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as TEL
import Data.Time (NominalDiffTime, UTCTime (..))
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Tuple.Extra (firstM, uncurry3)
import FeedRepeat.Lib
import FeedRepeat.Types
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.Header qualified as HTTP
import System.FilePath ((<.>), (</>))
import System.IO.Error (illegalOperationErrorType, mkIOError)
import Text.Atom.Feed qualified as Atom
import Text.Feed.Export qualified as Feed
import Text.Feed.Import qualified as Feed
import Text.Feed.Query qualified as Feed
import Text.Feed.Types qualified as Feed
import Prelude hiding (appendFile, readFile, writeFile)

requestTimeoutMicros :: Int
requestTimeoutMicros = 30_000_000 -- 30 sec

timerTolerance :: NominalDiffTime
timerTolerance = 5 * 60 -- 5 minutes tolerance for systemd timer imprecision

maxBodySize :: Int
maxBodySize = 10 * 1024 * 1024 -- 10 MB

ancientTime :: UTCTime
ancientTime = UTCTime (Time.fromGregorian 2000 1 1) 0

runTasksForSource :: (MonadApp m) => [FeedTask] -> URL -> m ()
runTasksForSource tasks sourceFeedUrl = do
  env <- ask

  -- parse output files
  (tasks, mOutputFeeds) <- unzip . catMaybes <$> traverse mParseOutputFile tasks

  -- get output file updates times
  let outputFeedsUpdated = flip map mOutputFeeds $ \case
        Nothing -> ancientTime
        Just outputFeed ->
          let updated = parseDate $ Atom.feedUpdated outputFeed
           in fromMaybe ancientTime updated

  -- skip run or get run task action for tasks
  (actions, cached) <-
    fmap (second and . unzip . catMaybes)
      . forM (zip3 tasks mOutputFeeds outputFeedsUpdated)
      . uncurry3
      $ getTaskAction env

  -- run actions
  unless (null actions) $ do
    let metadataFilePath = env.options.cacheDir </> sha256Hash sourceFeedUrl.toString <.> "json"
    metadata <- parseFeedMetadata' metadataFilePath
    mSourceFeedAndMetadata <- mFetchFeed sourceFeedUrl metadata cached
    forM_ (snd <$> mSourceFeedAndMetadata) $ saveFeedMetadata' metadataFilePath
    traverse_ ($ fst <$> mSourceFeedAndMetadata) actions
  where
    getTaskAction env task mOutputFeed outputFeedUpdated = do
      let minRunGapSeconds = fromIntegral task.minRunGapDays.toNum * Time.nominalDay - timerTolerance
      if Time.diffUTCTime env.startTime outputFeedUpdated < minRunGapSeconds
        then do
          outputPath <- outputFilePath task
          logInfo $ "Skipping run for output:" <> outputPath <> " URL: " <> sourceFeedUrl.redacted
          return Nothing
        else do
          cachePath <- cacheFilePath task
          eCachedFeed <-
            (Right <$> parseAtomFile cachePath) `catchError` \err -> do
              (case err of IOError _ -> logDebug; _ -> logWarn) $
                "Unable to parse cache file " <> cachePath <> ": " <> show err
              return $ Left err

          return $ Just $ (,isRight eCachedFeed) $ \mSourceFeed ->
            runTask task mSourceFeed eCachedFeed mOutputFeed `catchError` (logError . show)

    parseFeedMetadata' metadataFilePath =
      parseFeedMetadata metadataFilePath
        `catchError` \err -> do
          (case err of IOError _ -> logDebug; _ -> logWarn) $
            "Failed to read feed metadata: " <> show err
          return $ FeedMetadata Nothing Nothing

    saveFeedMetadata' metadataFilePath metadata =
      ( saveFeedMetadata metadataFilePath metadata
          >> logDebug ("Saved feed metadata: " <> metadataFilePath)
      )
        `catchError` \err ->
          logWarn $ "Failed to save feed metadata: " <> metadataFilePath <> show err

    mFetchFeed url metadata cached =
      (Just <$> fetchFeed url metadata cached) `catchError` \err -> do
        case err of
          FeedNotModifiedError -> logDebug $ "Feed not modified: " <> url.redacted <> ", using cached"
          FeedTooManyRequestsError -> logDebug $ "Feed too many requests: " <> url.redacted <> ", using cached"
          _ -> logWarn $ "Unable to fetch fresh feed: " <> url.redacted <> ", using cached: " <> show err
        return Nothing

    mParseOutputFile task = do
      outputPath <- outputFilePath task

      (Just . (task,) . Just <$> parseAtomFile outputPath) -- file parses, task runs
        `catchError` \case
          IOError err -> do
            -- file missing or unreadable, task still runs
            logDebug $ "Failed to read output feed: " <> show err
            return $ Just (task, Nothing)
          err -> do
            -- file corrupted, tasks does not run
            logWarn $ "Corrupted output feed: " <> show err
            return Nothing

    runTask task mSourceFeed eCachedFeed mOutputFeed = do
      let url = task.sourceFeedUrl
      logDebug $ "Processing: " <> url.redacted
      logDebug $
        ("Task params: saveSourceFeedEntries=" <> show task.saveSourceFeedEntries)
          <> (", repeatedEntryCount=" <> show task.repeatedEntryCount)
          <> (", minimumEntryAgeDays=" <> show task.minimumEntryAgeDays)
          <> (", minRunGapDays=" <> show task.minRunGapDays)

      cacheSourceFeed task mSourceFeed eCachedFeed >>= processSourceFeed task mOutputFeed

processSourceFeed :: (MonadApp m) => FeedTask -> Maybe Atom.Feed -> (Atom.Feed, [Atom.Entry]) -> m ()
processSourceFeed task mOutputFeed (sourceFeed, newEntries') = do
  -- merge source and output feeds
  mergedFeed <- case mOutputFeed of
    Nothing -> return sourceFeed
    Just outputFeed -> return $ mergeFeeds sourceFeed outputFeed

  let allEntries = Atom.feedEntries mergedFeed
  logDebug $ "Merged feed has " <> show (length allEntries) <> " entries"

  -- select entries
  env <- ask
  let timestamp = T.pack $ iso8601Show env.startTime
  selectedEntries' <- selectEntries task env.startTime allEntries newEntries'
  [selectedEntries, newEntries] <-
    traverse (traverse (resetEntryId timestamp)) [selectedEntries', newEntries']

  if null selectedEntries && null newEntries
    then logWarn "Selected no new entries or entries for repetition"
    else do
      logDebug $ "Selected " <> show (length selectedEntries) <> " entries for repetition"
      when task.passthroughNewEntries $
        logDebug ("Got " <> show (length newEntries) <> " new entries")

      -- merge entries
      let outputFeedEntries = maybe [] Atom.feedEntries mOutputFeed
      let combinedEntries = newEntries <> selectedEntries <> outputFeedEntries
      logDebug $
        unwords
          [ "Combined entries:",
            if task.passthroughNewEntries then show (length newEntries) <> " new +" else "\b",
            show (length selectedEntries) <> " repeated +",
            show (length outputFeedEntries) <> " existing =",
            show (length combinedEntries)
          ]

      -- create new output feed
      let resultFeed =
            (fromMaybe sourceFeed mOutputFeed)
              { Atom.feedUpdated = timestamp,
                Atom.feedEntries = combinedEntries
              }

      -- write new output feed
      let url = task.sourceFeedUrl
      content <-
        fromMaybeOrThrow (FeedRenderError url.redacted) . Feed.textFeed $ Feed.AtomFeed resultFeed
      outputPath <- outputFilePath task
      writeFileText outputPath content
      logDebug $ "Wrote to: " <> outputPath
      logInfo $ "Processed " <> url.redacted <> " successfully"
  where
    resetEntryId timestamp entry = do
      entryId <- mkUuidUrn
      return entry {Atom.entryId = entryId, Atom.entryUpdated = timestamp}

sha256Hash :: String -> String
sha256Hash = BSC.pack >>> Crypto.hash @_ @SHA256 >>> show

oldCacheFileName :: URL -> String
oldCacheFileName url = sha256Hash url.toString <> ".atom"

cacheFileName :: URL -> String -> String
cacheFileName url outputFilename =
  sha256Hash (url.toString <> ['\0'] <> outputFilename) <> ".atom"

cacheFilePath :: (MonadApp m) => FeedTask -> m FilePath
cacheFilePath task = do
  env <- ask
  return $ env.options.cacheDir </> cacheFileName task.sourceFeedUrl task.outputFilename

outputFilePath :: (MonadApp m) => FeedTask -> m FilePath
outputFilePath task = do
  env <- ask
  return $ env.options.outputDir </> task.outputFilename <> ".atom"

cacheSourceFeed :: (MonadApp m) => FeedTask -> Maybe Atom.Feed -> Either AppError Atom.Feed -> m (Atom.Feed, [Atom.Entry])
cacheSourceFeed task mSourceFeed eCachedFeed = do
  let url = task.sourceFeedUrl
  cachePath <- cacheFilePath task

  mergedFeed <-
    if
      | task.saveSourceFeedEntries,
        Just sourceFeed <- mSourceFeed,
        Right cachedFeed <- eCachedFeed ->
          pure $ mergeFeeds sourceFeed cachedFeed
      | Just sourceFeed <- mSourceFeed -> pure sourceFeed
      | otherwise -> case eCachedFeed of
          Right cachedFeed -> pure cachedFeed
          Left err -> throwError err

  when (isJust mSourceFeed) $
    case Feed.textFeed (Feed.AtomFeed mergedFeed) of
      Nothing -> logWarn $ "Failed to export feed for URL: " <> url.redacted
      Just txt ->
        (writeFileText cachePath txt >> logDebug ("Cached to: " <> cachePath))
          `catchError` (logWarn . ("Failed to write cache file: " <>) . show)

  return (mergedFeed, computeNewEntries task.passthroughNewEntries mSourceFeed eCachedFeed)

fetchFeed :: (MonadApp m) => URL -> FeedMetadata -> Bool -> m (Atom.Feed, FeedMetadata)
fetchFeed url metadata cached = do
  env <- ask
  (feed, metadata') <- fetchAndParse env.startTime env.httpManager env.options.userAgent url.toString
  logDebug $ "Fetched feed with " <> show (length $ Atom.feedEntries feed) <> " entries: " <> url.redacted
  return (feed, metadata')
  where
    fetchAndParse now man userAgent =
      (HTTP.parseRequest >>> tryOrThrow HTTPError)
        >=> (addHeaders userAgent >>> fetchWithRetry man >>> tryOrThrow HTTPError)
        >=> (checkForStatus HTTP.status304 >>> fromMaybeOrThrow FeedNotModifiedError)
        >=> (checkForStatus HTTP.status429 >>> fromMaybeOrThrow FeedTooManyRequestsError)
        >=> ( responseBodyAndMetadata
                >>> firstM (Feed.parseFeedSource >>> fromMaybeOrThrow (FeedParseError url.redacted))
            )
        >=> firstM (feedToAtom now url)

    addHeaders userAgent request =
      request
        { HTTP.responseTimeout = HTTP.responseTimeoutMicro requestTimeoutMicros,
          HTTP.requestHeaders =
            HTTP.requestHeaders request
              <> [(HTTP.hUserAgent, TE.encodeUtf8 userAgent)]
              <> [(HTTP.hIfModifiedSince, TE.encodeUtf8 lastMod) | cached, Just lastMod <- [metadata.lastModified]]
              <> [(HTTP.hIfNoneMatch, TE.encodeUtf8 etag) | cached, Just etag <- [metadata.etag]]
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

    responseBodyAndMetadata resp =
      ( HTTP.responseBody resp,
        FeedMetadata
          { etag = TE.decodeASCII' =<< lookup HTTP.hETag (HTTP.responseHeaders resp),
            lastModified = TE.decodeASCII' =<< lookup HTTP.hLastModified (HTTP.responseHeaders resp)
          }
      )

parseAtomFile :: (MonadApp m) => FilePath -> m Atom.Feed
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
        >=> Feed.parseFeedSource
        >>> fromMaybeOrThrow (FeedParseError filePath)

parseFeedMetadata :: (MonadApp m) => FilePath -> m FeedMetadata
parseFeedMetadata filePath =
  readFile filePath
    >>= ( Aeson.eitherDecode'
            >>> mapLeft (FeedMetadataParseError filePath)
            >>> liftEither
        )

saveFeedMetadata :: (MonadApp m) => FilePath -> FeedMetadata -> m ()
saveFeedMetadata filePath = Aeson.encode >>> writeFile filePath

logInfoIO, logErrorIO :: String -> IO ()
logInfoIO = runStdoutLoggingT . logInfo
logErrorIO = runStdoutLoggingT . logError

logDebug, logInfo, logWarn, logError :: (MonadLogger m) => String -> m ()
logDebug = logDebugN . T.pack
logInfo = logInfoN . T.pack
logWarn = logWarnN . T.pack
logError = logErrorN . T.pack

writeFileText :: (MonadApp m) => FilePath -> LT.Text -> m ()
writeFileText fp = TEL.encodeUtf8 >>> writeFile fp

migrateCacheFile :: (MonadApp m) => FilePath -> [FeedTask] -> m ()
migrateCacheFile cacheDir tasks = do
  let sourceFeedUrls = nubOrd $ map sourceFeedUrl tasks
  forM_ sourceFeedUrls $ \url -> do
    let oldFileName = cacheDir </> oldCacheFileName url
        tasksWithSource = [t | t <- tasks, t.sourceFeedUrl == url]
    exists <- doesFileExist oldFileName
    when exists $ do
      results <- forM tasksWithSource $ \task -> do
        let newFileName = cacheDir </> cacheFileName task.sourceFeedUrl task.outputFilename
        doesFileExist newFileName >>= \case
          True -> return True
          False ->
            ( copyFile oldFileName newFileName
                >> logInfo ("Migrated cache file: " <> oldFileName <> " to " <> newFileName)
                >> return True
            )
              `catchError` \(IOError e) -> do
                logWarn $ "Cache migration failed for " <> oldFileName <> ": " <> displayException e
                return False
      when (and results) $
        removeFile oldFileName `catchError` \(IOError e) ->
          logWarn $ "Failed to remove old cache file " <> oldFileName <> ": " <> displayException e

runAllTasks :: (MonadApp m) => [FeedTask] -> m ()
runAllTasks tasks = do
  env <- ask
  migrateCacheFile env.options.cacheDir tasks
  forM_ (nubOrd $ map sourceFeedUrl tasks) $ \url ->
    runTasksForSource (filter ((== url) . sourceFeedUrl) tasks) url
      `catchError` (logError . show)

checkDuplicateOutputs :: [FeedTask] -> IO [FeedTask]
checkDuplicateOutputs tasks =
  let outputFilenames = map outputFilename tasks
      duplicates = outputFilenames \\ nub outputFilenames
   in if null duplicates
        then return tasks
        else do
          logErrorIO "Duplicate output filenames found:"
          forM_ (nub duplicates) (logErrorIO . ("  " <>))
          return []
