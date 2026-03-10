module Day2Spec where

import Day2
import FileLoader
import Test.Hspec

spec :: Spec
spec = describe "Day 2" $ do
  bigInput <- runIO $ getLinesFromFile "test/day2_input.txt"

  it "load big input from file" $
    length bigInput `shouldBe` 2500

  it "parse round" $
    parseRound "A Y" `shouldBe` Just (Round (MyShape Paper) (OpponentShape Rock))

  it "get round outcome" $
    getRoundOutcome (Round (MyShape Paper) (OpponentShape Rock))
      `shouldBe` RoundOutcome Me (MyScore 8) (OpponentScore 1)

  it "get my total score (BIG input)" $
    getMyTotalScore bigInput `shouldBe` Just 12586

  it "get my planned total score (BIG input)" $
    getMyPlannedTotalScore bigInput `shouldBe` Just 13193
