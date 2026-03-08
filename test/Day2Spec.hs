module Day2Spec where

import qualified Data.Text as T
import Day2
import FileLoader
import Test.Hspec

spec :: Spec
spec = describe "Day 2" $ do
  it "load big input from file" $ do
    input <- bigInput
    length input `shouldBe` 2500

  it "parse round" $
    parseRound "A Y" `shouldBe` Just (Round (MyShape Paper) (OpponentShape Rock))

  it "get round outcome" $
    getRoundOutcome (Round (MyShape Paper) (OpponentShape Rock))
      `shouldBe` RoundOutcome Me (MyScore 8) (OpponentScore 1)

  it "get my total score (BIG input)" $ do
    input <- bigInput
    getMyTotalScore input `shouldBe` Just 12586

  it "get my planned total score (BIG input)" $ do
    input <- bigInput
    getMyPlannedTotalScore input `shouldBe` Just 13193

bigInput :: IO [T.Text]
bigInput = getLinesFromFile "test/day2_input.txt"
