{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Day3
import FileLoader
import NeatInterpolation (trimming)
import Test.Hspec

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
      `shouldBe` Just
        ( Rucksack
            (Item 'a' :| [Item 'b', Item 'c'])
            (Item 'D' :| [Item 'E', Item 'F'])
        )

  it "Rucksack repeatedItemsBetweenCompartments with NO repeated" $
    repeatedItemsBetweenCompartments
      ( Rucksack
          (Item 'a' :| [Item 'b', Item 'c'])
          (Item 'D' :| [Item 'A', Item 'F'])
      )
      `shouldBe` []

  it "Rucksack repeatedItemsBetweenCompartments with 1 repeated" $
    repeatedItemsBetweenCompartments
      ( Rucksack
          (Item 'a' :| [Item 'b', Item 'c'])
          (Item 'D' :| [Item 'a', Item 'F'])
      )
      `shouldBe` [Item 'a']

  it "Rucksack repeatedItemsBetweenCompartments with 2 repeated" $
    repeatedItemsBetweenCompartments
      ( Rucksack
          (Item 'a' :| [Item 'b', Item 'c'])
          (Item 'c' :| [Item 'C', Item 'a'])
      )
      `shouldBe` [Item 'a', Item 'c']

  it "getRepeatedItemsPrioritiesSum on small input" $
    getRepeatedItemsPrioritiesSum smallInput `shouldBe` Just 157

  it "getRepeatedItemsPrioritiesSum on BIG input" $
    getRepeatedItemsPrioritiesSum bigInput `shouldBe` Just 7903

smallInput :: [Text]
smallInput =
  T.splitOn
    "\n"
    [trimming|
      vJrwpWtwJgWrhcsFMMfFFhFp
      jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      PmmdzqPrVvPwwTWBwg
      wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      ttgJtRGJQctTZtZT
      CrZsJsPPZsGzwwsLwLmpwMDw
      |]
