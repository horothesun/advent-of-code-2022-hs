{-# LANGUAGE QuasiQuotes #-}

module Day1Spec where

import qualified Data.Text as T
import Day1
import FileLoader
import NeatInterpolation (trimming)
import Test.Hspec

spec :: Spec
spec = describe "Day 1" $ do
  it "load big input from file" $ do
    input <- bigInput
    length input `shouldBe` 2223

  it "parse small input" $
    parseElfPacks smallInput
      `shouldBe` Just
        [ ElfPack
            [ FoodItem $ Calories 1000
            , FoodItem $ Calories 2000
            , FoodItem $ Calories 3000
            ]
        , ElfPack
            [ FoodItem $ Calories 4000
            ]
        , ElfPack
            [ FoodItem $ Calories 5000
            , FoodItem $ Calories 6000
            ]
        , ElfPack
            [ FoodItem $ Calories 7000
            , FoodItem $ Calories 8000
            , FoodItem $ Calories 9000
            ]
        , ElfPack
            [ FoodItem $ Calories 10000
            ]
        ]

  it "total Calories carried by the Elf with most calories (small input)" $
    getElfWithMostCaloriesTotalCalories smallInput `shouldBe` Just 24000

  it "total Calories carried by the Elf with most calories (BIG input)" $ do
    input <- bigInput
    getElfWithMostCaloriesTotalCalories input `shouldBe` Just 70764

  it "total Calories carried by top 3 Elves with most calories (small input)" $
    getTop3ElvesWithMostCaloriesTotalCalories smallInput `shouldBe` Just 45000

  it "total Calories carried by top 3 Elves with most calories (BIG input)" $ do
    input <- bigInput
    getTop3ElvesWithMostCaloriesTotalCalories input `shouldBe` Just 203905

bigInput :: IO [T.Text]
bigInput = getLinesFromFile "test/day1_input.txt"

smallInput :: [T.Text]
smallInput =
  T.splitOn
    "\n"
    [trimming|
      1000
      2000
      3000

      4000

      5000
      6000

      7000
      8000
      9000

      10000
      |]
