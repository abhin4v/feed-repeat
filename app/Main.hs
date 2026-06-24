module Main where

import Control.Exception (IOException, displayException, throwIO, try)
import Control.Monad (filterM, unless, (>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (filterLogger, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Version (showVersion)
import Data.Yaml qualified as Yaml
import FeedRepeat.Lib
import FeedRepeat.Runner
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Options.Applicative qualified as Opt
import PackageInfo_feed_repeat qualified as PI
import System.Directory (canonicalizePath, createDirectoryIfMissing)
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
          <> Opt.help "Directory where cached Atom files will be stored, default: '.'"
      )
    <*> Opt.strOption
      ( Opt.long "user-agent"
          <> Opt.metavar "STRING"
          <> Opt.value ("feed-repeat/" <> T.pack (showVersion PI.version))
          <> Opt.help "User-Agent header to send in HTTP requests, default: 'feed-repeat/<version>'"
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
        [] -> logErrorIO "No valid tasks found" >> exitFailure
        validated
          | env.options.validateOnly ->
              unless env.options.quiet $ do
                logInfoIO $ "Config valid: " <> show (length validated) <> " tasks"
        validated ->
          runAllTasks validated
            & runExceptT
            & flip runReaderT env
            & filterLogger (enableLogging env.options)
            & runStdoutLoggingT
            >>= either (logErrorIO . show) pure

validateTasks :: FilePath -> [FeedTask] -> IO [FeedTask]
validateTasks outputDir =
  validateSsrf
    >=> validateOutputPaths outputDir
    >=> checkDuplicateOutputs

validateSsrf :: [FeedTask] -> IO [FeedTask]
validateSsrf tasks = flip filterM tasks $ \task -> do
  public <- checkPublicUrl task.sourceFeedUrl.toString
  unless public $ do
    logErrorIO $ "Private source feed URL found: " <> task.sourceFeedUrl.redacted
  return public

validateOutputPaths :: FilePath -> [FeedTask] -> IO [FeedTask]
validateOutputPaths outputDir tasks = flip filterM tasks $ \task -> do
  outputFP <- canonicalizePath $ outputDir </> task.outputFilename <.> "atom"
  let valid = splitDirectories outputDir `isPrefixOf` splitDirectories outputFP
  unless valid $ do
    logErrorIO $ "Output file is outside output directory: " <> outputFP
  return valid
