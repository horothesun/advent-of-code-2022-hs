module Day1Spec where

import Day1
import FileLoader
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "Day 1" $ do
  prop "" $
    \l -> reverse (reverse l) == (l :: [Int])

  it "load big input from file" $ do
    input <- getLinesFromFile "test/day1_input.txt"
    length input `shouldBe` 2223
