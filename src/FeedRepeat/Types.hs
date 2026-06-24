{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module FeedRepeat.Types
  ( URL (..),
    newURL,
    NonNegative (..),
    newNonNegative,
    FeedTask (..),
    AppError (..),
    Options (..),
    Env (..),
    App,
    FeedMetadata (..),
  )
where

import Control.Exception (IOException, displayException)
import Control.Monad.Except (ExceptT)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (ReaderT)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Hashable (Hashable (..))
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Network.HTTP.Client qualified as HTTP

data URL = URL {toString :: String, redacted :: String} deriving stock (Generic)

instance Eq URL where
  x == y = x.toString == y.toString

instance Ord URL where
  compare x y = compare x.toString y.toString

instance Hashable URL where
  hashWithSalt salt x = hashWithSalt salt x.toString
  hash x = hash x.toString

instance Show URL where
  show = redacted

newURL :: String -> Maybe URL
newURL url = do
  parsed <- HTTP.parseRequest url
  pure $ URL url (show $ HTTP.getUri parsed)

instance Aeson.FromJSON URL where
  parseJSON = Aeson.withText "String" $ \str ->
    case newURL (T.unpack str) of
      Nothing -> fail $ "Expect an URL, but found: " <> T.unpack str
      Just url -> return url

newtype NonNegative a = NonNegative {unNonNegative :: a}
  deriving stock (Eq)

instance HasField "toNum" (NonNegative a) a where
  getField = unNonNegative

instance (Show a) => Show (NonNegative a) where
  show (NonNegative a) = show a

newNonNegative :: (Ord a, Num a) => a -> Maybe (NonNegative a)
newNonNegative num = if num >= 0 then Just $ NonNegative num else Nothing

instance Aeson.FromJSON (NonNegative Int) where
  parseJSON = Aeson.withScientific "number" $ \num ->
    if num >= 0
      then case Scientific.toBoundedInteger num of
        Just n -> return $ NonNegative n
        Nothing -> fail $ "Expected non-negative int, but found: " <> show num
      else fail $ "Expected non-negative int, but found: " <> show num

instance Aeson.FromJSON (NonNegative Double) where
  parseJSON = Aeson.withScientific "number" $ \num ->
    if num >= 0
      then case Scientific.toBoundedRealFloat num of
        Right n -> return $ NonNegative n
        Left _ -> fail $ "Expected non-negative double, but found: " <> show num
      else fail $ "Expected non-negative double, but found: " <> show num

data FeedTask = FeedTask
  { sourceFeedUrl :: URL,
    outputFilename :: String,
    saveSourceFeedEntries :: Bool,
    repeatedEntryCount :: NonNegative Int,
    minimumEntryAgeDays :: NonNegative Int,
    minRunGapDays :: NonNegative Int,
    maxEntryCountPerDomain :: Maybe (NonNegative Int),
    selectionAlpha :: NonNegative Double,
    passthroughNewEntries :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON FeedTask where
  parseJSON = Aeson.withObject "FeedTask" $ \v ->
    FeedTask
      <$> v .: "sourceFeedUrl"
      <*> v .: "outputFilename"
      <*> v .: "saveSourceFeedEntries"
      <*> v .: "repeatedEntryCount"
      <*> v .: "minimumEntryAgeDays"
      <*> v .:? "minRunGapDays" .!= NonNegative 1
      <*> v .:? "maxEntryCountPerDomain"
      <*> v .:? "selectionAlpha" .!= NonNegative 1
      <*> v .:? "passthroughNewEntries" .!= False

data AppError
  = IOError IOException
  | FeedParseError FilePath
  | FeedRenderError FilePath
  | InvalidFormatError String FilePath
  | InvalidFeedUpdatedError
  | FeedNotModifiedError
  | FeedTooManyRequestsError
  | FeedMetadataParseError FilePath String
  | HTTPError HTTP.HttpException

instance Show AppError where
  show = \case
    IOError err -> "Failed to read/write file " <> displayException err
    FeedParseError filePath -> "Failed to parse: " <> filePath
    FeedRenderError filePath -> "Failed to render: " <> filePath
    InvalidFormatError format filePath -> "File is not in " <> format <> " format: " <> filePath
    InvalidFeedUpdatedError -> "Feed updated date absent"
    FeedNotModifiedError -> "Feed not modified"
    FeedTooManyRequestsError -> "Feed too many requests"
    FeedMetadataParseError filePath err -> "Failed to parse: " <> filePath <> ": " <> err
    HTTPError err -> "HTTP error: " <> displayException err

data Options = Options
  { configPath :: FilePath,
    outputDir :: FilePath,
    cacheDir :: FilePath,
    userAgent :: T.Text,
    validateOnly :: Bool,
    verbose :: Bool,
    quiet :: Bool
  }

data Env = Env {options :: Options, httpManager :: HTTP.Manager, startTime :: UTCTime}

type App a = ExceptT AppError (ReaderT Env (LoggingT IO)) a

data FeedMetadata = FeedMetadata {etag :: Maybe T.Text, lastModified :: Maybe T.Text}
  deriving (Show, Eq, Generic, Aeson.ToJSON, Aeson.FromJSON)
