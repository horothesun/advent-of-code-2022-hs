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
    parseRound "A Y" `shouldBe` Just Round{roundMe = Paper, roundOpponent = Rock}

  it "get round outcome" $
    getRoundOutcome Round{roundMe = Paper, roundOpponent = Rock}
      `shouldBe` RoundOutcome{roundOutcomeWinner = Me, roundOutcomeMe = Score 8, roundOutcomeOpponent = Score 1}

  it "get my total score (BIG input)" $ do
    input <- bigInput
    getMyTotalScore input `shouldBe` Just (Score 12586)

  it "get my planned total score (BIG input)" $ do
    input <- bigInput
    getMyPlannedTotalScore input `shouldBe` Just (Score 13193)

bigInput :: IO [T.Text]
bigInput = getLinesFromFile "test/day2_input.txt"
