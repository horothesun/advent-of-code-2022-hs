module Day3Spec where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Day3
import FileLoader
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "Day 3" $ do
  bigInput <- runIO $ getLinesFromFile "test/day3_input.txt"

  it "load big input from file" $
    length bigInput `shouldBe` 300

  it "Item 'a' to Item 'z' priorities are 1 to 26" $
    let items = traverse parseItem ['a' .. 'z']
     in fmap priority <$> items `shouldBe` Just (fmap Priority [1 .. 26])

  it "Item 'A' to Item 'Z' priorities are 27 to 52" $
    let items = traverse parseItem ['A' .. 'Z']
     in fmap priority <$> items `shouldBe` Just (fmap Priority [27 .. 52])

  it "parseRucksack \"\" returns Nothing" $
    parseRucksack "" `shouldBe` Nothing

  it "parseRucksack \"aBC\" returns Nothing" $
    parseRucksack "aBC" `shouldBe` Nothing

  it "parseRucksack \"abcDEF\" returns a valid Rucksack" $
    parseRucksack "abcDEF"
      `shouldBe` Just (Rucksack (Item 'a' :| [Item 'b', Item 'c']) (Item 'D' :| [Item 'E', Item 'F']))
