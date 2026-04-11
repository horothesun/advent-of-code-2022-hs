module Day5Spec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Stack as Stack
import Data.Text (Text)
import Day5
import FileLoader
import Test.Hspec
import Text.Megaparsec (runParser)

spec :: Spec
spec = describe "Day 5" $ do
  bigInput <- runIO $ getLinesFromFile "test/day5_input.txt"

  it "load big input from file" $
    length bigInput `shouldBe` 513

  it "emptyStacks 0 returns Nothing" $
    emptyStacks 0 `shouldBe` Nothing

  it "emptyStacks 1 returns the correct result" $
    emptyStacks 1 `shouldBe` Just (Stacks $ Stack.new :| [])

  it "emptyStacks 2 returns the correct result" $
    emptyStacks 2 `shouldBe` Just (Stacks $ Stack.new :| [Stack.new])

  it "parse stacks from small input" $
    let input =
          emptyText
            <> "    [D]    \n"
            <> "[N] [C]    \n"
            <> "[Z] [M] [P]\n"
            <> " 1   2   3 \n"
     in runParser crateItemGridP "" input
          `shouldBe` Right
            ( (NoCrate :| [CrateItem (Crate 'D'), NoCrate])
                :| [ CrateItem (Crate 'N') :| [CrateItem (Crate 'C'), NoCrate]
                   , CrateItem (Crate 'Z') :| [CrateItem (Crate 'M'), CrateItem (Crate 'P')]
                   ]
            )

  it "parse movement list from small input" $
    let input =
          emptyText
            <> "move 1 from 2 to 1\n"
            <> "move 3 from 1 to 3\n"
            <> "move 2 from 2 to 1\n"
            <> "move 1 from 1 to 2"
     in runParser movePlanP "" input
          `shouldBe` Right
            ( MovePlan
                [ Move (Count 1) (From 2) (To 1)
                , Move (Count 3) (From 1) (To 3)
                , Move (Count 2) (From 2) (To 1)
                , Move (Count 1) (From 1) (To 2)
                ]
            )

  it "parse entire small input" $
    let input =
          emptyText
            <> "    [D]    \n"
            <> "[N] [C]    \n"
            <> "[Z] [M] [P]\n"
            <> " 1   2   3 \n"
            <> "\n"
            <> "move 1 from 2 to 1\n"
            <> "move 3 from 1 to 3\n"
            <> "move 2 from 2 to 1\n"
            <> "move 1 from 1 to 2"
     in runParser allP "" input
          `shouldBe` Right
            ( Stacks
                ( Stack.push (Crate 'N') (Stack.push (Crate 'Z') Stack.new)
                    :| [ Stack.push (Crate 'D') (Stack.push (Crate 'C') (Stack.push (Crate 'M') Stack.new))
                       , Stack.push (Crate 'P') Stack.new
                       ]
                )
            , MovePlan
                [ Move (Count 1) (From 2) (To 1)
                , Move (Count 3) (From 1) (To 3)
                , Move (Count 2) (From 2) (To 1)
                , Move (Count 1) (From 1) (To 2)
                ]
            )

  it "Stack.fromList [1, 2, 3] == Stack 3 [1, 2, 3]" $
    Stack.fromList [1, 2, 3] `shouldBe` Stack.push 1 (Stack.push 2 (Stack.push 3 Stack.new))

  it "stacksFromCrateItemGrid on small input" $
    let grid :: NonEmpty (NonEmpty CrateItem)
        grid =
          (NoCrate :| [CrateItem (Crate 'D'), NoCrate])
            :| [ CrateItem (Crate 'N') :| [CrateItem (Crate 'C'), NoCrate]
               , CrateItem (Crate 'Z') :| [CrateItem (Crate 'M'), CrateItem (Crate 'P')]
               ]
     in stacksFromCrateItemGrid grid
          `shouldBe` Stacks
            ( Stack.push (Crate 'N') (Stack.push (Crate 'Z') Stack.new)
                :| [ Stack.push (Crate 'D') (Stack.push (Crate 'C') (Stack.push (Crate 'M') Stack.new))
                   , Stack.push (Crate 'P') Stack.new
                   ]
            )

emptyText :: Text
emptyText = ""
