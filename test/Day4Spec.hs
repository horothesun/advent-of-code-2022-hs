{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Day4
import FileLoader
import NeatInterpolation (trimming)
import Test.Hspec

spec :: Spec
spec = describe "Day 4" $ do
  bigInput <- runIO $ getLinesFromFile "test/day4_input.txt"

  it "load big input from file" $
    length bigInput `shouldBe` 1000

  it "4-5 is fully contained in 4-6" $
    let l = SectionRange (SectionId 4 :| [SectionId 5])
        r = SectionRange (SectionId 4 :| [SectionId 5, SectionId 6])
     in (l `isFullyContainedIn` r) `shouldBe` True

  it "5-5 is fully contained in 4-6" $
    let l = SectionRange (SectionId 5 :| [])
        r = SectionRange (SectionId 4 :| [SectionId 5, SectionId 6])
     in (l `isFullyContainedIn` r) `shouldBe` True

  it "6-6 is fully contained in 4-6" $
    let l = SectionRange (SectionId 6 :| [])
        r = SectionRange (SectionId 4 :| [SectionId 5, SectionId 6])
     in (l `isFullyContainedIn` r) `shouldBe` True

  it "4-6 is NOT fully contained in 6-6" $
    let l = SectionRange (SectionId 4 :| [SectionId 5, SectionId 6])
        r = SectionRange (SectionId 6 :| [])
     in (l `isFullyContainedIn` r) `shouldBe` False

  it "4-6 is NOT fully contained in 8-9" $
    let l = SectionRange (SectionId 4 :| [SectionId 5, SectionId 6])
        r = SectionRange (SectionId 8 :| [SectionId 9])
     in (l `isFullyContainedIn` r) `shouldBe` False

  it "8-9 is NOT fully contained in 4-6" $
    let l = SectionRange (SectionId 8 :| [SectionId 9])
        r = SectionRange (SectionId 4 :| [SectionId 5, SectionId 6])
     in (l `isFullyContainedIn` r) `shouldBe` False

  it "parseSectionRange \"3-5\" returns the correct SectionId NEL" $
    parseSectionRange "3-5" `shouldBe` Just (SectionRange $ SectionId 3 :| [SectionId 4, SectionId 5])

  it "parseAssignmentPair \"2-3,5-6\" returns the correct SectionId NEL pair" $
    parseAssignmentPair "2-3,5-6"
      `shouldBe` Just
        ( AssignmentPair
            (SectionRange $ SectionId 2 :| [SectionId 3])
            (SectionRange $ SectionId 5 :| [SectionId 6])
        )

  it "getAssignmentPairsWithFullyContainedRangeCount on small input" $
    getAssignmentPairsWithFullyContainedRangeCount smallInput `shouldBe` Just 2

  it "getAssignmentPairsWithFullyContainedRangeCount on BIG input" $
    getAssignmentPairsWithFullyContainedRangeCount bigInput `shouldBe` Just 532

  it "getAssignmentPairsWithOverlappingRangesCount on small input" $
    getAssignmentPairsWithOverlappingRangesCount smallInput `shouldBe` Just 4

  it "getAssignmentPairsWithOverlappingRangesCount on BIG input" $
    getAssignmentPairsWithOverlappingRangesCount bigInput `shouldBe` Just 854

smallInput :: [Text]
smallInput =
  T.splitOn
    "\n"
    [trimming|
      2-4,6-8
      2-3,4-5
      5-7,7-9
      2-8,3-7
      6-6,4-6
      2-6,4-8
      |]
