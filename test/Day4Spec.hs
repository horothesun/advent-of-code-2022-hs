{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

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

smallInput :: [Text]
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
