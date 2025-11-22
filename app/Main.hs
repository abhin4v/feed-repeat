{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main where

import Control.Applicative ((<|>))
import Control.Exception (IOException, displayException, try)
import Control.Monad (forM, join, mplus, when)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isRight)
import Data.Hashable (Hashable, hash)
import Data.List (nubBy, sortBy)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, maybeToList)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time
  ( NominalDiffTime,
    UTCTime (..),
    diffUTCTime,
    getCurrentTime,
    getCurrentTimeZone,
    utcToLocalTime,
  )
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
-- sampling where older entries have higher probability (using exponential decay with a
-- configurable half-life).
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
  try (createDirectoryIfMissing True options.outputDir) >>= \case
    Left (e :: IOException) -> do
      logMsg ERR $ "Failed to create output directory: " <> displayException e
      exitFailure
    Right _ ->
      try (createDirectoryIfMissing True options.cacheDir) >>= \case
        Left (e :: IOException) -> do
          logMsg ERR $ "Failed to create cache directory: " <> displayException e
          exitFailure
        Right _ -> run options

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
          mapM_ (\c -> let URL url = c.sourceFeedUrl in logMsg ERR $ "  " <> url) invalidTasks
          exitFailure
        else mapM_ (\c -> runTask c options.outputDir options.cacheDir) validTasks

minRunGapSeconds :: NominalDiffTime
minRunGapSeconds = 86400 -- one day

runTask :: FeedTask -> FilePath -> FilePath -> IO ()
runTask task outputDir cacheDir = do
  let URL url = task.sourceFeedUrl
  logMsg DBG $ "Processing: " <> url
  now <- getCurrentTime
  let outputPath = outputDir </> task.outputFilename <> ".atom"
  outputFeedResult <- parseAtomFile outputPath
  let outputFeedUpdated = case outputFeedResult of
        Left _ -> UTCTime (fromGregorian 2000 1 1) 0
        Right outputFeed -> fromMaybe now $ parseDate $ Atom.feedUpdated outputFeed
  if diffUTCTime now outputFeedUpdated < minRunGapSeconds
    then logMsg INF $ "Skipping run for URL: " <> url
    else
      fetchCacheFeed task.cacheSourceFeed task.sourceFeedUrl cacheDir >>= \case
        Left err -> logMsg ERR $ "Error fetching feed: " <> err
        Right sourceFeed -> do
          logMsg DBG $
            "Fetched feed with " <> show (length $ Atom.feedEntries sourceFeed) <> " entries"
          processSourceFeed task outputFeedResult sourceFeed outputDir

processSourceFeed :: FeedTask -> Either String Atom.Feed -> Atom.Feed -> FilePath -> IO ()
processSourceFeed task outputFeedResult sourceFeed outputDir = do
  -- merge source and output feeds
  mergedFeed <- case outputFeedResult of
    Left err -> logMsg DBG ("Failed to read output feed: " <> err) >> return sourceFeed
    Right outputFeed -> return $ mergeFeeds sourceFeed outputFeed

  let allEntries = Atom.feedEntries mergedFeed
  logMsg DBG $ "Merged feed has " <> show (length allEntries) <> " entries"

  -- select entries
  now <- getCurrentTime
  let timestamp = T.pack $ iso8601Show now
      minAgeSeconds = fromIntegral task.minimumEntryAgeDays * 86400
  selectedEntries <-
    selectEntries task.repeatedEntryCount minAgeSeconds allEntries
      >>= traverse
        ( \e -> do
            entryId <- mkUuidUrn
            return e {Atom.entryId = entryId, Atom.entryUpdated = timestamp}
        )
  logMsg DBG $ "Selected " <> show (length selectedEntries) <> " entries for repetition"

  -- merge entries
  let outputFeedEntries = case outputFeedResult of
        Left _ -> []
        Right outputFeed -> Atom.feedEntries outputFeed
  let combinedEntries = selectedEntries <> outputFeedEntries
  logMsg DBG $
    "Combined entries: "
      <> (show (length selectedEntries) <> " new + ")
      <> (show (length outputFeedEntries) <> " existing = ")
      <> show (length combinedEntries)

  -- create new output feed
  let resultFeed' = case outputFeedResult of
        Left _ -> sourceFeed
        Right outputFeed -> outputFeed
      resultFeed =
        resultFeed'
          { Atom.feedUpdated = T.pack $ iso8601Show now,
            Atom.feedEntries = combinedEntries
          }

  -- write new output feed
  let URL url = task.sourceFeedUrl
  case Feed.textFeed (Feed.AtomFeed resultFeed) of
    Nothing -> logMsg ERR $ "Failed to render feed for: " <> url
    Just txt -> do
      let outputPath = outputDir </> task.outputFilename <> ".atom"
      try (writeFile outputPath txt) >>= \case
        Left (e :: IOException) ->
          logMsg ERR $ "Failed to write output file: " <> displayException e
        Right _ -> logMsg INF $ "Processed " <> url <> " successfully"

mkUuidUrn :: IO T.Text
mkUuidUrn = T.pack . ("urn:uuid:" <>) . show <$> UUID.nextRandom

fetchCacheFeed :: Bool -> URL -> FilePath -> IO (Either String Atom.Feed)
fetchCacheFeed cache (URL url) cacheDir = do
  let fileName = show (hash url) <> ".atom"
      filePath = cacheDir </> fileName
  fetchFeed url >>= \case
    Left err | cache -> do
      logMsg WRN $ "Unable to fetch fresh feed for URL: " <> url <> ", using cached: " <> err
      parseAtomFile filePath
    Left err -> return $ Left err
    Right freshFeed -> do
      mergedFeed <-
        if cache
          then
            parseAtomFile filePath >>= \case
              Left _ -> return freshFeed
              Right savedFeed -> return $ mergeFeeds freshFeed savedFeed
          else return freshFeed

      when cache $
        case Feed.textFeed (Feed.AtomFeed mergedFeed) of
          Nothing -> logMsg WRN $ "Failed to export feed for URL: " <> url
          Just txt -> do
            try (writeFile filePath txt) >>= \case
              Left (e :: IOException) ->
                logMsg WRN $ "Failed to write cache file: " <> displayException e
              Right _ -> logMsg INF $ "Cached " <> filePath <> " for URL: " <> url

      return $ Right mergedFeed

fetchFeed :: String -> IO (Either String Atom.Feed)
fetchFeed url =
  try (HTTP.parseRequest url) >>= \case
    Left (e :: HTTP.HttpException) -> return $ Left $ "Invalid URL: " <> displayException e
    Right request -> do
      let request' =
            request
              { HTTP.responseTimeout = HTTP.responseTimeoutMicro requestTimeoutMicros,
                HTTP.requestHeaders =
                  HTTP.requestHeaders request <> [(HTTP.hUserAgent, "feed-repeat")]
              }
      try (HTTP.httpLBS request') >>= \case
        Left (e :: HTTP.HttpException) -> return $ Left $ "HTTP error: " <> displayException e
        Right response -> do
          let body = TL.fromStrict $ TE.decodeUtf8Lenient $ LBS.toStrict $ HTTP.getResponseBody response
          case Feed.parseFeedSource body of
            Nothing -> return $ Left $ "Failed to parse feed: " <> url
            Just feed ->
              feedToAtom feed >>= \case
                Nothing -> return $ Left $ "Failed to convert feed: " <> url
                Just atomFeed -> return $ Right atomFeed
  where
    requestTimeoutMicros = 30_000_000 -- 30 sec

feedToAtom :: Feed.Feed -> IO (Maybe Atom.Feed)
feedToAtom (Feed.AtomFeed af) = return $ Just af
feedToAtom feed = do
  feedUuid <- mkUuidUrn
  now <- getCurrentTime
  entries <-
    sortBy (comparing (Down . Atom.entryUpdated))
      <$> traverse (itemToAtomEntry now) (Feed.getFeedItems feed)
  let title = Feed.getFeedTitle feed
      link = Feed.getFeedHome feed
      updateDate = Feed.getFeedLastUpdate feed
      pubDate = Feed.getFeedPubDate feed
      feedId = fromMaybe feedUuid link
      mFeedUpdated = updateDate <|> pubDate <|> listToMaybe (map Atom.entryUpdated entries)
  return $ case mFeedUpdated of
    Nothing -> Nothing
    Just feedUpdated ->
      Just $
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
    mkLinks relsUrls = catMaybes $ map (\(rel, mUrl) -> mkLink rel <$> mUrl) relsUrls

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
    halfLifeDays = 7

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
         in if age > 0 then exp (realToFrac age / (86400 * halfLifeDays)) else 1

    select now es = do
      keys <- forM es $ \entry -> do
        r <- randomRIO (0, 1)
        return $ r ** (1 / (computeWeight now entry))
      return $ take n $ map fst $ sortBy (comparing (Down . snd)) $ zip es keys

parseAtomFile :: FilePath -> IO (Either String Atom.Feed)
parseAtomFile filePath = do
  content <- try $ readFile filePath
  case content of
    Left (e :: IOException) ->
      return . Left $ "Error reading " <> filePath <> ": " <> displayException e
    Right body -> case Feed.parseFeedString body of
      Nothing -> return . Left $ "Failed to parse Atom file " <> filePath
      Just feed -> case feed of
        Feed.AtomFeed af -> do
          logMsg DBG $
            ("Parsed Atom file " <> filePath <> " with ")
              <> (show (length $ Feed.getFeedItems feed) <> " entries")
          return $ Right af
        _ -> return $ Left $ "File is not in Atom format: " <> filePath

logMsg :: LogLevel -> String -> IO ()
logMsg level msg = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz now
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
  putStrLn $ timestamp <> " [" <> show level <> "] " <> msg

writeFile :: FilePath -> TL.Text -> IO ()
writeFile fp content = do
  let tmpFP = fp <> ".tmp"
  BS.writeFile tmpFP . TE.encodeUtf8 $ TL.toStrict content
  renameFile tmpFP fp

parseDate :: T.Text -> Maybe UTCTime
parseDate ds = do
  let rfc3339DateFormat1 = "%Y-%m-%dT%H:%M:%S%Z"
      rfc3339DateFormat2 = "%Y-%m-%dT%H:%M:%S%Q%Z"
      formats = [rfc3339DateFormat1, rfc3339DateFormat2, rfc822DateFormat]
  foldl1 mplus (map (\fmt -> parseTimeM True defaultTimeLocale fmt $ T.unpack ds) formats)
