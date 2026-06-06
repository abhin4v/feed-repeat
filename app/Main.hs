module Main where

import Control.Exception (IOException, displayException, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (LogLevel (..), filterLogger, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, nub, (\\))
import Data.List.Extra (nubOrd)
import Data.Maybe (catMaybes)
import Data.Time qualified as Time
import Data.Version (showVersion)
import Data.Yaml qualified as Yaml
import FeedRepeat.Lib
import FeedRepeat.Runner
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Options.Applicative qualified as Opt
import PackageInfo_feed_repeat qualified as PI
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (exitFailure)
import System.FilePath (splitDirectories, (<.>), (</>))
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
