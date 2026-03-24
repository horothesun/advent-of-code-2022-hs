{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Stack
import Data.Text (Text)
import qualified Data.Text as T
import Day5
import FileLoader
import NeatInterpolation (trimming)
import Test.Hspec

spec :: Spec
spec = describe "Day 5" $ do
  bigInput <- runIO $ getLinesFromFile "test/day5_input.txt"

  it "load big input from file" $
    length bigInput `shouldBe` 513

  it "emptyStacks 0 returns Nothing" $
    emptyStacks 0 `shouldBe` Nothing

  it "emptyStacks 1 returns the correct result" $
    emptyStacks 1 `shouldBe` Just (Stacks $ stackNew :| [])

  it "emptyStacks 2 returns the correct result" $
    emptyStacks 2 `shouldBe` Just (Stacks $ stackNew :| [stackNew])

  it "preProcessInput on small input" $
    let input :: [Text]
        input =
          [ "    [D]    "
          , "[N] [C]    "
          , "[Z] [M] [P]"
          ]
     in preProcessInput input
          `shouldBe` [ "[~] [D] [~]"
                     , "[N] [C] [~]"
                     , "[Z] [M] [P]"
                     ]

  it "stackFromList [1, 2, 3] == Stack 3 [1, 2, 3]" $
    stackFromList [1, 2, 3] `shouldBe` stackPush 1 (stackPush 2 (stackPush 3 stackNew))

  it "stacksFromCrateItemGrid on small input" $
    let grid :: NonEmpty (NonEmpty CrateItem)
        grid =
          (NoCrate :| [CrateItem (Crate 'D'), NoCrate])
            :| [ CrateItem (Crate 'N') :| [CrateItem (Crate 'C'), NoCrate]
               , CrateItem (Crate 'Z') :| [CrateItem (Crate 'M'), CrateItem (Crate 'P')]
               ]
     in stacksFromCrateItemGrid grid
          `shouldBe` Stacks
            ( stackPush (Crate 'N') (stackPush (Crate 'Z') stackNew)
                :| [ stackPush (Crate 'D') (stackPush (Crate 'C') (stackPush (Crate 'M') stackNew))
                   , stackPush (Crate 'P') stackNew
                   ]
            )

smallInput :: [Text]
smallInput =
  T.splitOn
    "\n"
    [trimming|
      1000
      2000
      3000
      |]
