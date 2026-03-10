module Day3Spec where

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
