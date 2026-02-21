module Day2Spec where

import Day2
import FileLoader
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "Day 2" $ do
  prop "" $
    \l -> reverse (reverse l) == (l :: [Int])

  it "load big input from file" $ do
    input <- getLinesFromFile "test/day2_input.txt"
    length input `shouldBe` 2500
