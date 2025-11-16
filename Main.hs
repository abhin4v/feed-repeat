{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (IOException, displayException, try)
import Control.Monad (forM, join)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Either (isRight)
import Data.Hashable (hash)
import Data.List (findIndex, nubBy, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID.V4 (nextRandom)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Options.Applicative qualified as Opt
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Random (randomRIO)
import Text.Atom.Feed qualified as Atom
import Text.Feed.Export qualified as Feed
import Text.Feed.Import qualified as Feed
import Text.Feed.Query qualified as Feed
import Text.Feed.Types qualified as Feed

data LogLevel = Info | Error | Debug deriving (Show)

data Config = Config
  { source :: String,
    output :: String,
    cache :: Bool,
    repeatEntryCount :: Int
  }
  deriving (Show, Eq, Generic, Aeson.FromJSON)

data Options = Options
  { configPath :: FilePath,
    outputDir :: FilePath
  }

main :: IO ()
main = do
  options <-
    Opt.execParser $
      Opt.info
        optionsParser
        ( Opt.fullDesc
            <> Opt.progDesc "Feed repeater tool"
            <> Opt.header "feed-repeat"
        )
  createResult <- try $ createDirectoryIfMissing True (outputDir options)
  case createResult of
    Left e -> do
      logMsg Error $
        "Failed to create output directory: " <> displayException (e :: IOException)
      exitFailure
    Right _ -> do
      result <-
        Yaml.decodeFileEither (configPath options) :: IO (Either Yaml.ParseException [Config])
      case result of
        Left err -> do
          logMsg Error $ "Error reading config: " <> show err
          exitFailure
        Right configs | null configs -> logMsg Error "No configs found in file" >> exitFailure
        Right configs -> do
          validationResults <- forM configs $ \c -> do
            res <- try $ HTTP.parseRequest (source c) :: IO (Either HTTP.HttpException HTTP.Request)
            return (c, isRight res)
          let validConfigs = map fst $ filter snd validationResults
          let invalidConfigs = map fst $ filter (not . snd) validationResults
          if not (null invalidConfigs)
            then do
              logMsg Error "Invalid URLs in config:"
              mapM_ (\c -> logMsg Error $ "  " <> source c) invalidConfigs
              exitFailure
            else mapM_ (\c -> processConfig c (outputDir options)) validConfigs

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
      Opt.<**> Opt.helper

processConfig :: Config -> FilePath -> IO ()
processConfig config outputDir = do
  logMsg Debug $ "Processing config for " <> source config
  sourceFeedResult <- saveFeed (cache config) (source config)
  case sourceFeedResult of
    Left err -> logMsg Error $ "Error fetching feed: " <> err
    Right sourceFeed -> do
      logMsg Debug $ "Fetched feed with " <> show (length $ Atom.feedEntries sourceFeed) <> " entries"
      let outputPath = output config <> ".atom"
      outputFeedResult <- readOutputFile outputDir outputPath
      case outputFeedResult of
        Left err -> logMsg Debug $ "Failed to read existing feed: " <> err
        Right outputFeed ->
          logMsg Debug $
            "Read existing feed with " <> show (length $ Atom.feedEntries outputFeed) <> " entries"
      mergedFeed <- case outputFeedResult of
        Left _ -> return sourceFeed
        Right outputFeed -> mergeFeeds sourceFeed outputFeed
      let allEntries = Atom.feedEntries mergedFeed
      logMsg Debug $ "Merged feed has " <> show (length allEntries) <> " entries"
      selectedEntries <- selectEntries (repeatEntryCount config) allEntries
      logMsg Debug $ "Selected " <> show (length selectedEntries) <> " entries for repetition"
      now <- getCurrentTime
      let timestampString = T.pack $ iso8601Show now
      timestampedSelectedEntries <- forM selectedEntries $ \e -> do
        uuid <- nextRandom
        let newId = T.pack $ "urn:uuid:" <> show uuid
        return e {Atom.entryId = newId, Atom.entryUpdated = timestampString}
      logMsg Debug "Assigned UUIDs and updated timestamps to selected entries"
      outputEntries <- case outputFeedResult of
        Left _ -> return []
        Right outputFeed -> return $ Atom.feedEntries outputFeed
      let combinedEntries = timestampedSelectedEntries ++ outputEntries
      logMsg Debug $
        "Combined entries: "
          <> show (length timestampedSelectedEntries)
          <> " new + "
          <> show (length outputEntries)
          <> " existing = "
          <> show (length combinedEntries)
      resultFeed <- case outputFeedResult of
        Left _ -> return mergedFeed {Atom.feedEntries = combinedEntries}
        Right outputFeed -> return outputFeed {Atom.feedEntries = combinedEntries}
      case Feed.textFeed (Feed.AtomFeed resultFeed) of
        Nothing -> logMsg Error "Failed to export feed"
        Just txt -> do
          writeResult <- try $ writeFile (outputDir </> outputPath) (TL.unpack txt)
          case writeResult of
            Left e -> logMsg Error $ "Failed to write output file: " <> displayException (e :: IOException)
            Right _ ->
              logMsg Info $
                "Processed "
                  <> source config
                  <> " successfully, wrote "
                  <> show (length combinedEntries)
                  <> " entries"

saveFeed :: Bool -> String -> IO (Either String Atom.Feed)
saveFeed cache url = do
  result <- fetchFeed url
  case result of
    Left err -> return $ Left err
    Right feed -> do
      mergedFeed <-
        if cache
          then do
            let cacheFileName = show (hash url) <> ".atom"
            readResult <- readOutputFile "." cacheFileName
            case readResult of
              Left _ -> return feed
              Right savedFeed -> mergeFeeds savedFeed feed
          else return feed
      let fileName = show (hash url) <> ".atom"
      case Feed.textFeed (Feed.AtomFeed mergedFeed) of
        Nothing -> return $ Left "Failed to export feed as text"
        Just txt -> do
          writeResult <- try $ writeFile fileName (TL.unpack txt)
          case writeResult of
            Left e -> return $ Left $ "Failed to write cache file: " <> displayException (e :: IOException)
            Right _ -> return $ Right mergedFeed

fetchFeed :: String -> IO (Either String Atom.Feed)
fetchFeed url = do
  req <- try $ HTTP.parseRequest url
  case req of
    Left e -> return $ Left $ "Invalid URL: " <> displayException (e :: HTTP.HttpException)
    Right request -> do
      let request' =
            request
              { HTTP.responseTimeout = HTTP.responseTimeoutMicro 10_000_000,
                HTTP.requestHeaders = HTTP.requestHeaders request <> [(HTTP.hUserAgent, "feed-repeat")]
              }
      resp <- try $ HTTP.httpLBS request'
      case resp of
        Left e -> return $ Left $ "HTTP error: " <> displayException (e :: HTTP.HttpException)
        Right response -> do
          let body = T.unpack $ TE.decodeUtf8Lenient $ BS.toStrict $ HTTP.getResponseBody response
          case Feed.parseFeedString body of
            Nothing -> return $ Left "Failed to parse feed"
            Just feed -> do atomFeed <- feedToAtom feed; return $ Right atomFeed

feedToAtom :: Feed.Feed -> IO Atom.Feed
feedToAtom feed = do
  let title = Feed.getFeedTitle feed
      link = Feed.getFeedHome feed
      pubDate = Feed.getFeedPubDate feed
      feedId = fromMaybe "" link
      feedTitle = Atom.TextString title
      feedUpdated = fromMaybe "" pubDate
      baseFeed = Atom.nullFeed feedId feedTitle feedUpdated
  entries <- mapM itemToAtomEntry (Feed.getFeedItems feed)
  return baseFeed {Atom.feedEntries = entries, Atom.feedLinks = [Atom.nullLink feedId]}
  where
    itemToAtomEntry :: Feed.Item -> IO Atom.Entry
    itemToAtomEntry item = case item of
      Feed.AtomItem atomEntry -> return atomEntry
      _ -> do
        let title = Feed.getItemTitle item
            link = Feed.getItemLink item
            pubDate = join (Feed.getItemPublishDate item :: Maybe (Maybe UTCTime))
            desc = Feed.getItemDescription item
            entryId = fromMaybe "" link
            entryTitle = Atom.TextString $ fromMaybe "" title
            entryUpdated = T.pack $ maybe "" iso8601Show pubDate
        let entry =
              (Atom.nullEntry entryId entryTitle entryUpdated)
                { Atom.entryLinks = [Atom.nullLink $ fromMaybe "" link],
                  Atom.entryContent = Atom.HTMLContent <$> desc
                }
        if T.null (Atom.entryId entry)
          then do
            uuid <- nextRandom
            return entry {Atom.entryId = T.pack $ "urn:uuid:" <> show uuid}
          else return entry

mergeFeeds :: Atom.Feed -> Atom.Feed -> IO Atom.Feed
mergeFeeds saved new = do
  let allEntries = Atom.feedEntries saved <> Atom.feedEntries new
  let sortedEntries = sortBy (comparing (Down . Atom.entryUpdated)) allEntries
  let uniqueEntries =
        nubBy
          (\a b -> Feed.getItemLink (Feed.AtomItem a) == Feed.getItemLink (Feed.AtomItem b))
          sortedEntries
  return saved {Atom.feedEntries = uniqueEntries}

selectEntries :: Int -> [Atom.Entry] -> IO [Atom.Entry]
selectEntries n entries = do
  now <- getCurrentTime
  let weights = map (computeWeight now) entries
  select n entries weights []
  where
    halfLifeDays :: Double
    halfLifeDays = 7

    computeWeight :: UTCTime -> Atom.Entry -> Double
    computeWeight now entry = case Feed.getItemPublishDate (Feed.AtomItem entry) of
      Nothing -> 1
      Just Nothing -> 1
      Just (Just updated) ->
        let age = diffUTCTime now updated
         in if age > 0 then exp (realToFrac age / (86400 * halfLifeDays)) else 1

    select :: Int -> [Atom.Entry] -> [Double] -> [Atom.Entry] -> IO [Atom.Entry]
    select 0 _ _ acc = return acc
    select k es ws acc = do
      let total = sum ws
      r <- randomRIO (0, total)
      let cumulative = scanl (+) 0 ws
      let idx = min (length es - 1) $ fromMaybe 0 $ findIndex (> r) cumulative
      let selected = es !! idx
      let newEs = take idx es <> drop (idx + 1) es
      let newWs = take idx ws <> drop (idx + 1) ws
      select (k - 1) newEs newWs (selected : acc)

readOutputFile :: FilePath -> String -> IO (Either String Atom.Feed)
readOutputFile dir name = do
  let filePath = dir </> name
  content <- try $ readFile filePath
  case content of
    Left e ->
      return . Left $
        "File error reading " <> filePath <> ": " <> displayException (e :: IOException)
    Right body -> case Feed.parseFeedString body of
      Nothing ->
        return $
          Left $
            "Failed to parse Atom file " <> filePath <> ", content start: " <> take 200 body
      Just feed -> case feed of
        Feed.AtomFeed af -> do
          logMsg Debug $
            "Parsed feed with "
              <> show (length $ Feed.getFeedItems $ Feed.AtomFeed af)
              <> " items, "
              <> show (length $ Atom.feedEntries af)
              <> " entries"
          return $ Right af
        _ -> return $ Left $ "File is not Atom: " <> filePath

logMsg :: LogLevel -> String -> IO ()
logMsg level msg = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz now
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
  putStrLn $ timestamp <> " [" <> show level <> "] " <> msg
