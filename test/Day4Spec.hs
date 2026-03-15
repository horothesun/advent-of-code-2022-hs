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
