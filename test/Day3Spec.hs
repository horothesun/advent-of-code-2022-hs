module Day3Spec where

import qualified Data.Text as T
import Day3
import FileLoader
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "Day 3" $ do
  it "load big input from file" $ do
    input <- bigInput
    length input `shouldBe` 300

bigInput :: IO [T.Text]
bigInput = getLinesFromFile "test/day3_input.txt"
