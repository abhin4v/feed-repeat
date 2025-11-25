module Main where

import Control.Monad (replicateM, (>=>))
import Control.Monad.Except (runExceptT)
import Data.List (sort)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text qualified as T
import Lib
import Test.Hspec
import Test.QuickCheck
import Text.Atom.Feed qualified as Atom
import Text.Feed.Types qualified as Feed
import Text.RSS.Syntax qualified as RSS

main :: IO ()
main = hspec $ do
  describe "parseDate" $ do
    it "parses valid RFC3339 format with timezone" $ do
      let result = parseDate "2025-11-22T10:30:45UTC"
      result `shouldNotBe` Nothing

    it "parses valid RFC822 format" $ do
      let result = parseDate "Sat, 22 Nov 2025 10:30:45 GMT"
      result `shouldNotBe` Nothing

    it "parses RFC3339 with microseconds" $ do
      let result = parseDate "2025-11-22T10:30:45.123456UTC"
      result `shouldNotBe` Nothing

    it "parses dates with Z timezone indicator" $ do
      let result = parseDate "2025-11-22T10:30:45Z"
      result `shouldNotBe` Nothing

    it "parses uppercase and lowercase month names" $ do
      let result = parseDate "Sat, 22 nov 2025 10:30:45 GMT"
      result `shouldNotBe` Nothing

    it "returns Nothing for malformed dates" $ do
      let result = parseDate "invalid-date"
      result `shouldBe` Nothing

    it "returns Nothing for empty string" $ do
      let result = parseDate ""
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
      result <- selectEntries 0 (60 * 60 * 24 * 3) Nothing []
      length result `shouldBe` 0

    it "returns at most n entries" $ do
      let entry1 =
            (Atom.nullEntry "id1" (Atom.TextString "entry1") "Sun, 01 Jan 2025 10:30:45 GMT")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/1")]
              }
          entry2 =
            (Atom.nullEntry "id2" (Atom.TextString "entry2") "Sun, 01 Jan 2025 10:30:45 GMT")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/2")]
              }
          entry3 =
            (Atom.nullEntry "id3" (Atom.TextString "entry3") "Sun, 01 Jan 2025 10:30:45 GMT")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/3")]
              }
          entries = [entry1, entry2, entry3]
      result <- selectEntries 1 (60 * 60 * 24 * 365) Nothing entries
      length result `shouldSatisfy` (<= 1)

    it "filters out entries newer than minimum age" $ do
      let oldEntry =
            (Atom.nullEntry "id1" (Atom.TextString "old") "2020-01-01T10:30:45Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/old")]
              }
          newEntry =
            (Atom.nullEntry "id2" (Atom.TextString "new") "2025-11-25T10:30:45Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/new")]
              }
          entries = [oldEntry, newEntry]
      result <- selectEntries 1 (60 * 60 * 24 * 3) Nothing entries
      case result of
        [entry] -> Atom.entryId entry `shouldBe` "id1"
        _ -> expectationFailure $ "Expected exactly 1 entry, got " <> show (length result)

    it "limits entries per domain when maxEntryCountPerDomain is set" $ do
      let entry1 =
            (Atom.nullEntry "id1" (Atom.TextString "entry1") "2020-01-01T10:30:45Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/1")]
              }
          entry2 =
            (Atom.nullEntry "id2" (Atom.TextString "entry2") "2020-01-02T10:30:45Z")
              { Atom.entryLinks = [(Atom.nullLink "http://example.com/2")]
              }
          entry3 =
            (Atom.nullEntry "id3" (Atom.TextString "entry3") "2020-01-03T10:30:45Z")
              { Atom.entryLinks = [(Atom.nullLink "http://other.com/1")]
              }
          entries = [entry1, entry2, entry3]
      result <- selectEntries 3 0 (Just 1) entries
      length result `shouldBe` 2
      let domains =
            mapMaybe (listToMaybe . Atom.entryLinks >=> extractDomain . Atom.linkHref) result
      length domains `shouldBe` 2
      sort domains `shouldBe` ["example.com", "other.com"]

  describe "feedToAtom" $ do
    it "preserves feed ID when converting Atom" $ do
      let entry = Atom.nullEntry "entry-id" (Atom.TextString "Entry") "2025-11-22T10:00:00Z"
          atomFeed =
            (Atom.nullFeed "preserved-id" (Atom.TextString "Test") "2025-11-22T10:00:00Z")
              { Atom.feedEntries = [entry]
              }
      result <- runExceptT $ feedToAtom (Feed.AtomFeed atomFeed)
      case result of
        Left _ -> expectationFailure "feedToAtom failed"
        Right feed -> do
          Atom.feedId feed `shouldBe` "preserved-id"
          length (Atom.feedEntries feed) `shouldBe` 1

    it "preserves feed title when converting Atom" $ do
      let atomFeed = Atom.nullFeed "test-id" (Atom.TextString "My Feed Title") "2025-11-22T10:00:00Z"
      result <- runExceptT $ feedToAtom (Feed.AtomFeed atomFeed)
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
      result <- runExceptT $ feedToAtom (Feed.AtomFeed atomFeed)
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
      result <- runExceptT $ feedToAtom (Feed.RSSFeed rss)
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
      result <- runExceptT $ feedToAtom (Feed.RSSFeed rss)
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
      result <- runExceptT $ feedToAtom (Feed.RSSFeed rss)
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
      result <- runExceptT $ feedToAtom (Feed.RSSFeed rss)
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
      result <- runExceptT $ feedToAtom (Feed.RSSFeed rss)
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
      result <- runExceptT $ feedToAtom (Feed.RSSFeed rss)
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
      result <- runExceptT $ feedToAtom (Feed.RSSFeed rss)
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
      result <- runExceptT $ feedToAtom (Feed.RSSFeed rss)
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
      result <- runExceptT $ feedToAtom (Feed.RSSFeed rss)
      case result of
        Left err -> expectationFailure $ "feedToAtom failed: " <> show err
        Right atomFeed -> Atom.feedUpdated atomFeed `shouldBe` "2025-11-20T15:45:00Z"

  describe "selectEntries distribution" $ do
    it "favors older entries according to exponential weight (QuickCheck)" $ do
      ioProperty $ do
        let validYears = concat $ replicate 22 [1980 .. 2025]
            entries =
              zipWith
                ( \idx y ->
                    let entryId = T.pack $ show y ++ "_" ++ show idx
                     in ( Atom.nullEntry
                            entryId
                            (Atom.TextString $ T.pack $ "Entry from " ++ show y)
                            (T.pack $ show y ++ "-01-01T00:00:00Z")
                        )
                          { Atom.entryLinks = [(Atom.nullLink $ "http://example.com/" <> entryId)]
                          }
                )
                [1 ..]
                validYears
        if null entries
          then discard
          else do
            selections <- replicateM 10 (selectEntries 100 0 Nothing entries)
            let selectedIds = [Atom.entryId e | sel <- selections, e <- sel]
                selectedYears = map (read . T.unpack . T.takeWhile (/= '_')) selectedIds

            if null selectedYears
              then discard
              else do
                let decadeCounts1980s = length $ filter (\y -> 1980 <= y && y < 1990) selectedYears
                    yearCount = length selectedYears
                    pct1980s = (decadeCounts1980s * 100) `div` yearCount
                return
                  . cover 95 (pct1980s >= 95) "from 1980s"
                  $ pct1980s >= 95
