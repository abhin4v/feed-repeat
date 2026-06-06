module Main where

import Control.Monad (replicateM, (>=>))
import Control.Monad.Except (runExceptT)
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import FeedRepeat.Lib
import SsrfSpec (ssrfSpec)
import Test.Hspec
import Test.QuickCheck hiding (NonNegative)
import Text.Atom.Feed qualified as Atom
import Text.Feed.Query qualified as Feed
import Text.Feed.Types qualified as Feed
import Text.RSS.Syntax qualified as RSS

mkTask :: Int -> Int -> Maybe Int -> FeedTask
mkTask count minAgeDays maxPerDomain =
  FeedTask
    { sourceFeedUrl = fromMaybe (error "Impossible") $ newURL "http://example.com/feed",
      outputFilename = "test-output.xml",
      saveSourceFeedEntries = False,
      repeatedEntryCount = newNonNegative' count,
      minimumEntryAgeDays = newNonNegative' minAgeDays,
      minRunGapDays = newNonNegative' 1,
      maxEntryCountPerDomain = newNonNegative' <$> maxPerDomain,
      selectionAlpha = newNonNegative' 1,
      passthroughNewEntries = False
    }

newNonNegative' :: (Ord a, Num a) => a -> NonNegative a
newNonNegative' = fromMaybe (error "Impossible") . newNonNegative

newURL' :: String -> URL
newURL' = fromMaybe (error "Impossible") . newURL

testFeedURL :: URL
testFeedURL = newURL' "http://example.com/feed"

parseUTC :: String -> UTCTime
parseUTC = fromMaybe (error "Impossible") . iso8601ParseM

epoch :: UTCTime
epoch = parseUTC "1970-01-01T00:00:00Z"

farFutureNow :: UTCTime
farFutureNow = parseUTC "2025-06-01T00:00:00Z"

main :: IO ()
main = hspec $ do
  describe "parseDate" $ do
    describe "RFC3339 formats" $ do
      it "parses RFC3339 with UTC timezone" $ do
        let result = parseDate "2025-11-22T10:30:45UTC"
        result `shouldNotBe` Nothing

      it "parses RFC3339 with Z timezone indicator" $ do
        let result = parseDate "2025-11-22T10:30:45Z"
        result `shouldNotBe` Nothing

      it "parses RFC3339 with +00:00 timezone offset" $ do
        let result = parseDate "2025-11-22T10:30:45+00:00"
        result `shouldNotBe` Nothing

      it "parses RFC3339 with -05:00 timezone offset" $ do
        let result = parseDate "2025-11-22T10:30:45-05:00"
        result `shouldNotBe` Nothing

      it "parses RFC3339 with microseconds and UTC" $ do
        let result = parseDate "2025-11-22T10:30:45.123456UTC"
        result `shouldNotBe` Nothing

      it "parses RFC3339 with microseconds and Z" $ do
        let result = parseDate "2025-11-22T10:30:45.999999Z"
        result `shouldNotBe` Nothing

      it "parses RFC3339 with milliseconds" $ do
        let result = parseDate "2025-11-22T10:30:45.123UTC"
        result `shouldNotBe` Nothing

    describe "RFC822 formats" $ do
      it "parses RFC822 with GMT timezone" $ do
        let result = parseDate "Sat, 22 Nov 2025 10:30:45 GMT"
        result `shouldNotBe` Nothing

      it "parses RFC822 with numeric timezone offset" $ do
        let result = parseDate "Sat, 22 Nov 2025 10:30:45 +0000"
        result `shouldNotBe` Nothing

      it "parses RFC822 with negative timezone offset" $ do
        let result = parseDate "Sat, 22 Nov 2025 10:30:45 -0500"
        result `shouldNotBe` Nothing

      it "parses RFC822 with PST timezone" $ do
        let result = parseDate "Sat, 22 Nov 2025 10:30:45 PST"
        result `shouldNotBe` Nothing

      it "parses RFC822 with different day abbreviations" $ do
        let days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
        mapM_ (\day -> parseDate (T.pack $ day <> ", 22 Nov 2025 10:30:45 GMT") `shouldNotBe` Nothing) days

    describe "Simple date formats (no time)" $ do
      it "parses simple ISO8601 date" $ do
        let result = parseDate "2025-11-22"
        result `shouldNotBe` Nothing

      it "parses date with single-digit month and day" $ do
        let result = parseDate "2025-1-5"
        result `shouldNotBe` Nothing

      it "parses date with double-digit month and day" $ do
        let result = parseDate "2025-01-05"
        result `shouldNotBe` Nothing

    describe "Date formats with month names" $ do
      it "parses date with full month name" $ do
        let result = parseDate "17 Jul 2022 00:00:00 GMT"
        result `shouldNotBe` Nothing

      it "parses date with full month name and day abbreviation" $ do
        let result = parseDate "Mon, 08 December 2025 11:07:49 +0000"
        result `shouldNotBe` Nothing

      it "parses date with lowercase month names" $ do
        let result = parseDate "Sat, 22 nov 2025 10:30:45 GMT"
        result `shouldNotBe` Nothing

      it "parses date with different month abbreviations" $ do
        let months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
        mapM_ (\month -> parseDate (T.pack $ "22 " <> month <> " 2025 10:30:45 GMT") `shouldNotBe` Nothing) months

    describe "Edge cases and boundary dates" $ do
      it "parses leap year date (Feb 29)" $ do
        let result = parseDate "2024-02-29T10:30:45Z"
        result `shouldNotBe` Nothing

      it "parses year 2000" $ do
        let result = parseDate "2000-01-01T00:00:00Z"
        result `shouldNotBe` Nothing

      it "parses recent dates" $ do
        let result = parseDate "2025-12-30T23:59:59Z"
        result `shouldNotBe` Nothing

      it "parses dates at midnight" $ do
        let result = parseDate "2025-11-22T00:00:00Z"
        result `shouldNotBe` Nothing

      it "parses dates at end of day" $ do
        let result = parseDate "2025-11-22T23:59:59Z"
        result `shouldNotBe` Nothing

    describe "Invalid date formats" $ do
      it "returns Nothing for malformed dates" $ do
        let result = parseDate "invalid-date"
        result `shouldBe` Nothing

      it "returns Nothing for empty string" $ do
        let result = parseDate ""
        result `shouldBe` Nothing

      it "returns Nothing for random text" $ do
        let result = parseDate "not a date"
        result `shouldBe` Nothing

      it "returns Nothing for invalid month" $ do
        let result = parseDate "2025-13-01T10:30:45Z"
        result `shouldBe` Nothing

      it "returns Nothing for invalid day" $ do
        let result = parseDate "2025-11-31T10:30:45Z"
        result `shouldBe` Nothing

      it "returns Nothing for malformed timestamp" $ do
        let result = parseDate "2025-11-22T25:70:99Z"
        result `shouldBe` Nothing

      it "returns Nothing for only date part without time" $ do
        let result = parseDate "2025-11-22 not valid"
        result `shouldBe` Nothing

  describe "mergeFeeds" $ do
    it "deduplicates entries by link" $ do
      let entry1 =
            (Atom.nullEntry "id1" (Atom.TextString "entry1") "2025-11-22T10:00:00Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/1") {Atom.linkRel = Just $ Left "alternate"}]
              }
          entry2 =
            (Atom.nullEntry "id2" (Atom.TextString "entry1-duplicate") "2025-11-22T09:00:00Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/1") {Atom.linkRel = Just $ Left "alternate"}]
              }
          feed1 =
            (Atom.nullFeed "feed1" (Atom.TextString "Feed 1") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = [entry1]
              }
          feed2 =
            (Atom.nullFeed "feed2" (Atom.TextString "Feed 2") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = [entry2]
              }
          merged = mergeFeeds feed1 feed2
      length (Atom.feedEntries merged) `shouldBe` 1

    it "preserves all entries when no duplicates" $ do
      let entry1 =
            (Atom.nullEntry "id1" (Atom.TextString "entry1") "2025-11-22T10:00:00Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/1") {Atom.linkRel = Just $ Left "alternate"}]
              }
          entry2 =
            (Atom.nullEntry "id2" (Atom.TextString "entry2") "2025-11-22T09:00:00Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/2") {Atom.linkRel = Just $ Left "alternate"}]
              }
          feed1 =
            (Atom.nullFeed "feed1" (Atom.TextString "Feed 1") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = [entry1]
              }
          feed2 =
            (Atom.nullFeed "feed2" (Atom.TextString "Feed 2") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = [entry2]
              }
          merged = mergeFeeds feed1 feed2
      length (Atom.feedEntries merged) `shouldBe` 2

    it "sorts entries by timestamp (newest first)" $ do
      let entry1 =
            (Atom.nullEntry "id1" (Atom.TextString "entry1") "2025-11-22T09:00:00Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/1") {Atom.linkRel = Just $ Left "alternate"}]
              }
          entry2 =
            (Atom.nullEntry "id2" (Atom.TextString "entry2") "2025-11-22T10:00:00Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/2") {Atom.linkRel = Just $ Left "alternate"}]
              }
          feed1 =
            (Atom.nullFeed "feed1" (Atom.TextString "Feed 1") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = [entry1]
              }
          feed2 =
            (Atom.nullFeed "feed2" (Atom.TextString "Feed 2") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = [entry2]
              }
          merged = mergeFeeds feed1 feed2
      case Atom.feedEntries merged of
        [] -> expectationFailure "merged feed has no entries"
        (firstEntry : _) -> Atom.entryUpdated firstEntry `shouldBe` "2025-11-22T10:00:00Z"

    it "returns first feed as base when merging" $ do
      let feed1 =
            (Atom.nullFeed "feed1" (Atom.TextString "Feed 1") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = []
              }
          feed2 =
            (Atom.nullFeed "feed2" (Atom.TextString "Feed 2") "2025-11-22T09:00:00Z")
              { Atom.feedEntries = []
              }
          merged = mergeFeeds feed1 feed2
      Atom.feedId merged `shouldBe` "feed1"

  describe "selectEntries" $ do
    it "returns empty list when entries list is empty" $ do
      (repeated, new) <- selectEntries (mkTask 0 3 Nothing) epoch farFutureNow []
      length repeated `shouldBe` 0
      length new `shouldBe` 0

    it "returns at most n entries" $ do
      let entry1 =
            (Atom.nullEntry "id1" (Atom.TextString "entry1") "Sun, 01 Jan 2025 10:30:45 GMT")
              { Atom.entryLinks = [Atom.nullLink "http://example.com/1"]
              }
          entry2 =
            (Atom.nullEntry "id2" (Atom.TextString "entry2") "Sun, 01 Jan 2025 10:30:45 GMT")
              { Atom.entryLinks = [Atom.nullLink "http://example.com/2"]
              }
          entry3 =
            (Atom.nullEntry "id3" (Atom.TextString "entry3") "Sun, 01 Jan 2025 10:30:45 GMT")
              { Atom.entryLinks = [Atom.nullLink "http://example.com/3"]
              }
          entries = [entry1, entry2, entry3]
      (repeated, _) <- selectEntries (mkTask 1 365 Nothing) epoch farFutureNow entries
      length repeated `shouldSatisfy` (<= 1)

    it "filters out entries newer than minimum age" $ do
      let oldEntry =
            (Atom.nullEntry "id1" (Atom.TextString "old") "2020-01-01T10:30:45Z")
              { Atom.entryLinks = [Atom.nullLink "http://example.com/old"]
              }
          newEntry =
            (Atom.nullEntry "id2" (Atom.TextString "new") "2025-11-25T10:30:45Z")
              { Atom.entryLinks = [Atom.nullLink "http://example.com/new"]
              }
          entries = [oldEntry, newEntry]
      (repeated, _) <- selectEntries (mkTask 1 3 Nothing) epoch farFutureNow entries
      case repeated of
        [entry] -> Atom.entryId entry `shouldBe` "id1"
        _ -> expectationFailure $ "Expected exactly 1 entry, got " <> show (length repeated)

    it "limits entries per domain when maxEntryCountPerDomain is set" $ do
      let entry1 =
            (Atom.nullEntry "id1" (Atom.TextString "entry1") "2020-01-01T10:30:45Z")
              { Atom.entryLinks = [Atom.nullLink "http://example.com/1"]
              }
          entry2 =
            (Atom.nullEntry "id2" (Atom.TextString "entry2") "2020-01-02T10:30:45Z")
              { Atom.entryLinks = [Atom.nullLink "http://example.com/2"]
              }
          entry3 =
            (Atom.nullEntry "id3" (Atom.TextString "entry3") "2020-01-03T10:30:45Z")
              { Atom.entryLinks = [Atom.nullLink "http://other.com/1"]
              }
          entries = [entry1, entry2, entry3]
      (repeated, _) <- selectEntries (mkTask 3 0 (Just 1)) epoch farFutureNow entries
      length repeated `shouldBe` 2
      let domains =
            mapMaybe (listToMaybe . Atom.entryLinks >=> extractDomain . Atom.linkHref) repeated
      length domains `shouldBe` 2
      sort domains `shouldBe` ["example.com", "other.com"]

  describe "feedToAtom" $ do
    it "preserves feed ID when converting Atom" $ do
      let entry = Atom.nullEntry "entry-id" (Atom.TextString "Entry") "2025-11-22T10:00:00Z"
          atomFeed =
            (Atom.nullFeed "preserved-id" (Atom.TextString "Test") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = [entry]
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.AtomFeed atomFeed)
      case result of
        Left _ -> expectationFailure "feedToAtom failed"
        Right feed -> do
          Atom.feedId feed `shouldBe` "preserved-id"
          length (Atom.feedEntries feed) `shouldBe` 1

    it "preserves feed title when converting Atom" $ do
      let atomFeed = Atom.nullFeed "test-id" (Atom.TextString "My Feed Title") "2025-11-22T10:00:00Z"
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.AtomFeed atomFeed)
      case result of
        Left _ -> expectationFailure "feedToAtom failed"
        Right feed -> case Atom.feedTitle feed of
          Atom.TextString title -> title `shouldBe` "My Feed Title"
          _ -> expectationFailure "unexpected title type"

    it "preserves feed entries when converting Atom" $ do
      let entry1 = Atom.nullEntry "id1" (Atom.TextString "Entry 1") "2025-11-22T10:00:00Z"
          entry2 = Atom.nullEntry "id2" (Atom.TextString "Entry 2") "2025-11-22T09:00:00Z"
          atomFeed =
            (Atom.nullFeed "test-id" (Atom.TextString "Test") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = [entry1, entry2]
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.AtomFeed atomFeed)
      case result of
        Left _ -> expectationFailure "feedToAtom failed"
        Right feed -> length (Atom.feedEntries feed) `shouldBe` 2

    it "converts RSS feed to Atom" $ do
      let item =
            (RSS.nullItem "RSS Item")
              { RSS.rssItemLink = Just "http://example.com/item",
                RSS.rssItemDescription = Just "An item from RSS",
                RSS.rssItemPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT"
              }
          channel =
            (RSS.nullChannel "My RSS Feed" "http://example.com")
              { RSS.rssDescription = "Test RSS channel",
                RSS.rssPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT",
                RSS.rssItems = [item]
              }
          rss =
            (RSS.nullRSS "My RSS Feed" "http://example.com")
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> case Atom.feedTitle atomFeed of
          Atom.TextString title -> title `shouldBe` "My RSS Feed"
          _ -> expectationFailure "unexpected title type"

    it "converts RSS with multiple items" $ do
      let item1 =
            (RSS.nullItem "First")
              { RSS.rssItemLink = Just "http://example.com/1",
                RSS.rssItemDescription = Just "First item",
                RSS.rssItemPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT"
              }
          item2 =
            (RSS.nullItem "Second")
              { RSS.rssItemLink = Just "http://example.com/2",
                RSS.rssItemDescription = Just "Second item",
                RSS.rssItemPubDate = Just "Sat, 21 Nov 2025 10:00:00 GMT"
              }
          channel =
            (RSS.nullChannel "Multi Feed" "http://example.com")
              { RSS.rssDescription = "Feed with items",
                RSS.rssPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT",
                RSS.rssItems = [item1, item2]
              }
          rss =
            (RSS.nullRSS "Multi Feed" "http://example.com")
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> length (Atom.feedEntries atomFeed) `shouldBe` 2

    it "preserves RSS item publish dates on conversion" $ do
      let item =
            (RSS.nullItem "Dated")
              { RSS.rssItemLink = Just "http://example.com/dated",
                RSS.rssItemDescription = Just "Item with date",
                RSS.rssItemPubDate = Just "Fri, 21 Nov 2025 14:30:00 GMT"
              }
          channel =
            (RSS.nullChannel "Dated Feed" "http://example.com")
              { RSS.rssDescription = "Feed",
                RSS.rssPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT",
                RSS.rssItems = [item]
              }
          rss =
            (RSS.nullRSS "Dated Feed" "http://example.com")
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> case Atom.feedEntries atomFeed of
          [] -> expectationFailure "No entries in converted feed"
          (entry : _) -> Atom.entryUpdated entry `shouldBe` "2025-11-21T14:30:00Z"

    it "converts RSS item title to Atom entry title" $ do
      let item =
            (RSS.nullItem "Test Item Title")
              { RSS.rssItemLink = Just "http://example.com/item",
                RSS.rssItemPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT"
              }
          channel =
            (RSS.nullChannel "Test Feed" "http://example.com")
              { RSS.rssPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT",
                RSS.rssItems = [item]
              }
          rss =
            (RSS.nullRSS "Test Feed" "http://example.com")
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> case Atom.feedEntries atomFeed of
          [] -> expectationFailure "No entries in converted feed"
          (entry : _) -> case Atom.entryTitle entry of
            Atom.TextString title -> title `shouldBe` "Test Item Title"
            _ -> expectationFailure "unexpected title type"

    it "converts RSS item link to Atom entry link" $ do
      let testLink = "http://example.com/article-123"
          item =
            (RSS.nullItem "Article")
              { RSS.rssItemLink = Just testLink,
                RSS.rssItemPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT"
              }
          channel =
            (RSS.nullChannel "Blog" "http://example.com")
              { RSS.rssPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT",
                RSS.rssItems = [item]
              }
          rss =
            (RSS.nullRSS "Blog" "http://example.com")
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> case Atom.feedEntries atomFeed of
          [] -> expectationFailure "No entries in converted feed"
          (entry : _) ->
            let links = Atom.entryLinks entry
                altLinks = filter (\l -> Atom.linkRel l == Just (Left "alternate")) links
             in case altLinks of
                  [] -> expectationFailure "No alternate link found in entry"
                  (link : _) -> Atom.linkHref link `shouldBe` testLink

    it "converts RSS item description to Atom entry content" $ do
      let testDesc = "This is the item description"
          item =
            (RSS.nullItem "Story")
              { RSS.rssItemDescription = Just testDesc,
                RSS.rssItemPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT"
              }
          channel =
            (RSS.nullChannel "News" "http://example.com")
              { RSS.rssPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT",
                RSS.rssItems = [item]
              }
          rss =
            (RSS.nullRSS "News" "http://example.com")
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> case Atom.feedEntries atomFeed of
          [] -> expectationFailure "No entries in converted feed"
          (entry : _) -> case Atom.entrySummary entry of
            Just (Atom.HTMLString summary) -> summary `shouldBe` testDesc
            _ -> expectationFailure "No summary found or wrong type"

    it "preserves multiple item properties during conversion" $ do
      let item1 =
            (RSS.nullItem "Item One")
              { RSS.rssItemLink = Just "http://example.com/one",
                RSS.rssItemDescription = Just "First description",
                RSS.rssItemPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT"
              }
          item2 =
            (RSS.nullItem "Item Two")
              { RSS.rssItemLink = Just "http://example.com/two",
                RSS.rssItemDescription = Just "Second description",
                RSS.rssItemPubDate = Just "Fri, 21 Nov 2025 10:00:00 GMT"
              }
          channel =
            (RSS.nullChannel "Multi Feed" "http://example.com")
              { RSS.rssPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT",
                RSS.rssItems = [item1, item2]
              }
          rss =
            (RSS.nullRSS "Multi Feed" "http://example.com")
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> do
          let entries = Atom.feedEntries atomFeed
          length entries `shouldBe` 2
          case entries of
            (entry1 : entry2 : _) -> do
              -- Check first entry
              case Atom.entryTitle entry1 of
                Atom.TextString t -> t `shouldBe` "Item One"
                _ -> expectationFailure "Entry 1 title type incorrect"
              case Atom.entrySummary entry1 of
                Just (Atom.HTMLString s) -> s `shouldBe` "First description"
                _ -> expectationFailure "Entry 1 summary incorrect"
              -- Check second entry
              case Atom.entryTitle entry2 of
                Atom.TextString t -> t `shouldBe` "Item Two"
                _ -> expectationFailure "Entry 2 title type incorrect"
              case Atom.entrySummary entry2 of
                Just (Atom.HTMLString s) -> s `shouldBe` "Second description"
                _ -> expectationFailure "Entry 2 summary incorrect"
            _ -> expectationFailure "Unexpected number of entries"

    it "converts RSS feed link to Atom feed link" $ do
      let feedLink = "http://myblog.example.com"
          channel =
            (RSS.nullChannel "My Blog" feedLink)
              { RSS.rssPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT",
                RSS.rssItems = []
              }
          rss =
            (RSS.nullRSS "My Blog" feedLink)
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed ->
          let links = Atom.feedLinks atomFeed
              altLinks = filter (\l -> Atom.linkRel l == Just (Left "alternate")) links
           in case altLinks of
                [] -> expectationFailure "No alternate feed link found"
                (link : _) -> Atom.linkHref link `shouldBe` feedLink

    it "converts RSS channel pubDate to Atom feed updated" $ do
      let testDate = "Fri, 20 Nov 2025 15:45:00 GMT"
          channel =
            (RSS.nullChannel "Dated Feed" "http://example.com")
              { RSS.rssPubDate = Just testDate,
                RSS.rssItems = []
              }
          rss =
            (RSS.nullRSS "Dated Feed" "http://example.com")
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow testFeedURL (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> Atom.feedUpdated atomFeed `shouldBe` "2025-11-20T15:45:00Z"

  describe "feedToAtom link normalization" $ do
    let mkAtomFeedWithEntryLink linkHref =
          let entry =
                (Atom.nullEntry "id1" (Atom.TextString "Entry") "2025-11-22T10:00:00Z")
                  { Atom.entryLinks = [(Atom.nullLink linkHref) {Atom.linkRel = Just $ Left "alternate"}]
                  }
           in (Atom.nullFeed "feed-id" (Atom.TextString "Test") "2025-11-22T10:00:00Z")
                { Atom.feedEntries = [entry]
                }
        getFirstEntryLink atomFeed = case Atom.feedEntries atomFeed of
          (entry : _) -> Feed.getItemLink $ Feed.AtomItem entry
          _ -> Nothing
        feedUrl = newURL' "https://example.com/blog/feed/"

    it "preserves absolute http links" $ do
      let atomFeed = mkAtomFeedWithEntryLink "http://other.com/article"
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just "http://other.com/article"

    it "preserves absolute https links" $ do
      let atomFeed = mkAtomFeedWithEntryLink "https://other.com/article"
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just "https://other.com/article"

    it "resolves relative links against feed URL" $ do
      let atomFeed = mkAtomFeedWithEntryLink "/posts/my-article"
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just "https://example.com/posts/my-article"

    it "resolves relative links without leading slash" $ do
      let atomFeed = mkAtomFeedWithEntryLink "posts/my-article"
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just "https://example.com/blog/posts/my-article"

    it "resolves relative links with fragment against feed URL" $ do
      let atomFeed = mkAtomFeedWithEntryLink "/posts/my-article#some"
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just "https://example.com/posts/my-article#some"

    it "resolves relative links with query against feed URL" $ do
      let atomFeed = mkAtomFeedWithEntryLink "/posts/my-article?some"
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just "https://example.com/posts/my-article?some"

    it "resolves relative links with only fragment" $ do
      let atomFeed = mkAtomFeedWithEntryLink "#my-article"
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just "https://example.com/blog/#my-article"

    it "resolves relative links with only query" $ do
      let atomFeed = mkAtomFeedWithEntryLink "?some"
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just "https://example.com/blog/?some"

    it "percent-encodes spaces in absolute links" $ do
      let atomFeed = mkAtomFeedWithEntryLink "http://other.com/my article"
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just "http://other.com/my%20article"

    it "preserves empty links" $ do
      let atomFeed = mkAtomFeedWithEntryLink ""
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> getFirstEntryLink feed `shouldBe` Just ""

    it "normalizes feed-level links" $ do
      let atomFeed =
            (Atom.nullFeed "feed-id" (Atom.TextString "Test") "2025-11-22T10:00:00Z")
              { Atom.feedLinks = [(Atom.nullLink "/feed.atom") {Atom.linkRel = Just $ Left "self"}]
              }
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.AtomFeed atomFeed)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right feed -> case Atom.feedLinks feed of
          (link : _) -> Atom.linkHref link `shouldBe` "https://example.com/feed.atom"
          _ -> expectationFailure "No feed links found"

    it "resolves relative RSS item links against feed URL" $ do
      let item =
            (RSS.nullItem "RSS Item")
              { RSS.rssItemLink = Just "/rss-article",
                RSS.rssItemPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT"
              }
          channel =
            (RSS.nullChannel "Feed" "http://example.com")
              { RSS.rssPubDate = Just "Sat, 22 Nov 2025 10:00:00 GMT",
                RSS.rssItems = [item]
              }
          rss =
            (RSS.nullRSS "Feed" "http://example.com")
              { RSS.rssChannel = channel
              }
      result <- runExceptT $ feedToAtom farFutureNow feedUrl (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> case Atom.feedEntries atomFeed of
          (entry : _) ->
            let altLinks = filter (\l -> Atom.linkRel l == Just (Left "alternate")) (Atom.entryLinks entry)
             in case altLinks of
                  (link : _) -> Atom.linkHref link `shouldBe` "https://example.com/rss-article"
                  _ -> expectationFailure "No alternate link found"
          _ -> expectationFailure "No entries found"

  describe "selectEntries passthroughNewEntries" $ do
    let mkEntry eid published =
          (Atom.nullEntry eid (Atom.TextString eid) published)
            { Atom.entryLinks = [Atom.nullLink $ "http://example.com/" <> eid],
              Atom.entryPublished = Just published
            }

    it "includes entries newer than outputFeedUpdated when enabled" $ do
      let oldEntry = mkEntry "old" "2020-01-01T10:00:00Z"
          newEntry = mkEntry "new" "2025-11-25T10:00:00Z"
          outputUpdated = parseUTC "2025-01-01T00:00:00Z"
          task = (mkTask 1 365 Nothing) {passthroughNewEntries = True}
      (_, new) <- selectEntries task outputUpdated farFutureNow [oldEntry, newEntry]
      map Atom.entryId new `shouldSatisfy` elem "new"

    it "does not passthrough entries older than outputFeedUpdated when enabled" $ do
      let oldEntry = mkEntry "old" "2024-01-01T10:00:00Z"
          task = (mkTask 0 0 Nothing) {passthroughNewEntries = True}
          outputUpdated = parseUTC "2025-01-01T00:00:00Z"
      (repeated, new) <- selectEntries task outputUpdated farFutureNow [oldEntry]
      length (repeated <> new) `shouldBe` 0

    it "does not passthrough new entries when feature is disabled" $ do
      let newEntry = mkEntry "new" "2025-11-25T10:00:00Z"
          task = (mkTask 0 365 Nothing) {passthroughNewEntries = False}
          outputUpdated = parseUTC "2025-01-01T00:00:00Z"
      (repeated, new) <- selectEntries task outputUpdated farFutureNow [newEntry]
      length (repeated <> new) `shouldBe` 0

    it "ignores entries without published date when enabled" $ do
      let noDateEntry =
            (Atom.nullEntry "no-date" (Atom.TextString "no-date") "2025-11-25T10:00:00Z")
              { Atom.entryLinks = [Atom.nullLink "http://example.com/no-date"],
                Atom.entryPublished = Nothing
              }
          task = (mkTask 0 365 Nothing) {passthroughNewEntries = True}
          outputUpdated = parseUTC "2025-01-01T00:00:00Z"
      (repeated, new) <- selectEntries task outputUpdated farFutureNow [noDateEntry]
      length (repeated <> new) `shouldBe` 0

    it "ignores entries with unparseable published date when enabled" $ do
      let badDateEntry =
            (Atom.nullEntry "bad-date" (Atom.TextString "bad-date") "2025-11-25T10:00:00Z")
              { Atom.entryLinks = [Atom.nullLink "http://example.com/bad-date"],
                Atom.entryPublished = Just "not-a-date"
              }
          task = (mkTask 0 365 Nothing) {passthroughNewEntries = True}
          outputUpdated = parseUTC "2025-01-01T00:00:00Z"
      (repeated, new) <- selectEntries task outputUpdated farFutureNow [badDateEntry]
      length (repeated <> new) `shouldBe` 0

    it "deduplicates entries appearing in both passthrough and repeated selection" $ do
      let entry = mkEntry "dup" "2025-06-01T10:00:00Z"
          task = (mkTask 1 0 Nothing) {passthroughNewEntries = True}
          outputUpdated = parseUTC "2025-01-01T00:00:00Z"
      (repeated, new) <- selectEntries task outputUpdated farFutureNow [entry]
      length (repeated <> new) `shouldBe` 1

    it "passes through multiple new entries when enabled" $ do
      let new1 = mkEntry "new1" "2025-11-25T10:00:00Z"
          new2 = mkEntry "new2" "2025-11-26T10:00:00Z"
          new3 = mkEntry "new3" "2025-11-27T10:00:00Z"
          task = (mkTask 0 365 Nothing) {passthroughNewEntries = True}
          outputUpdated = parseUTC "2025-01-01T00:00:00Z"
      (repeated, new) <- selectEntries task outputUpdated farFutureNow [new1, new2, new3]
      length repeated `shouldBe` 0
      sort (map Atom.entryId new) `shouldBe` ["new1", "new2", "new3"]

  describe "SSRF protection" ssrfSpec

  describe "selectEntries distribution" $ do
    let validYears = concat $ replicate 22 [1980 .. 2024]
        entries =
          zipWith
            ( \idx year ->
                let entryId = T.pack $ show year ++ "_" ++ show idx
                 in ( Atom.nullEntry
                        entryId
                        (Atom.TextString $ T.pack $ "Entry from " ++ show year)
                        (T.pack $ show year ++ "-01-01T00:00:00Z")
                    )
                      { Atom.entryLinks = [Atom.nullLink $ "http://example.com/" <> entryId]
                      }
            )
            [1 ..]
            validYears
        task = mkTask 100 0 Nothing

    it "favors older entries according to exponential weight when alpha = 1" $ do
      ioProperty $ do
        if null entries
          then discard
          else do
            selections <- replicateM 10 $ fst <$> selectEntries task epoch farFutureNow entries
            let selectedIds = [Atom.entryId e | sel <- selections, e <- sel]
                selectedYears = map (read . T.unpack . T.takeWhile (/= '_')) selectedIds

            if null selectedYears
              then discard
              else do
                let yearCount = length selectedYears
                    decadeCounts1980s = length $ filter (\y -> 1980 <= y && y < 1990) selectedYears
                    pct1980s = (decadeCounts1980s * 100) `div` yearCount
                    decadeCounts2010s = length $ filter (\y -> 2010 <= y && y < 2020) selectedYears
                    pct2010s = (decadeCounts2010s * 100) `div` yearCount

                return
                  . cover 95 (pct1980s >= 95) "over 95% from 1980s"
                  . cover 0 (pct2010s <= 1) "below 1% from 2010s"
                  $ pct1980s >= 95 -- number of items from 1980s is over 95%
                    && pct2010s <= 1 -- number of items from 2010s is below 1%
    it "does not favor older entries according to exponential weight when alpha = 0" $ do
      ioProperty $ do
        if null entries
          then discard
          else do
            selections <- replicateM 10 $ fst <$> selectEntries (task {selectionAlpha = newNonNegative' 0}) epoch farFutureNow entries
            let selectedIds = [Atom.entryId e | sel <- selections, e <- sel]
                selectedYears = map (read . T.unpack . T.takeWhile (/= '_')) selectedIds

            if null selectedYears
              then discard
              else do
                let yearCount = length selectedYears
                    decadeCounts1980s = length $ filter (\y -> 1980 <= y && y < 1990) selectedYears
                    pct1980s = (decadeCounts1980s * 100) `div` yearCount
                    decadeCounts2010s = length $ filter (\y -> 2010 <= y && y < 2020) selectedYears
                    pct2010s = (decadeCounts2010s * 100) `div` yearCount

                return
                  . cover 20 (pct1980s <= 30) "below 30% from 1980s"
                  . cover 15 (pct2010s >= 18) "over 18% from 2010s"
                  $ pct1980s <= 30 -- number of items from 1980s is below 30%
                    && pct2010s >= 18 -- number of items from 2010s is over 18%
