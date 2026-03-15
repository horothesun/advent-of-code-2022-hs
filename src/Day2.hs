{-# LANGUAGE DerivingVia #-}

module Day2 where

import Data.Semigroup (Sum (..))
import Data.Text (Text)
import qualified Data.Text as T

newtype MyScore = MyScore Int
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup, Monoid) via (Sum Int)
newtype OpponentScore = OpponentScore Int
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup, Monoid) via (Sum Int)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

data Round = Round MyShape OpponentShape deriving (Eq, Show)
newtype MyShape = MyShape Shape deriving (Eq, Show)
newtype OpponentShape = OpponentShape Shape deriving (Eq, Show)

data Winner = Me | Opponent | Draw deriving (Eq, Show)

data RoundOutcome = RoundOutcome Winner MyScore OpponentScore deriving (Eq, Show)

data RoundPlan = RoundPlan OpponentShape Winner deriving (Eq, Show)

rawScore :: Shape -> Int
rawScore Rock = 1
rawScore Paper = 2
rawScore Scissors = 3

roundOutcomeMyScore :: RoundOutcome -> MyScore
roundOutcomeMyScore (RoundOutcome _ me _) = me

roundFromPlan :: RoundPlan -> Round
roundFromPlan (RoundPlan o@(OpponentShape opponent) desiredWinner) = Round me o
 where
  me = myShape desiredWinner
  myShape Me = MyShape $ winningShapeAgainst opponent
  myShape Opponent = MyShape $ losingShapeAgainst opponent
  myShape Draw = MyShape opponent

winningShapeAgainst :: Shape -> Shape
winningShapeAgainst Rock = Paper
winningShapeAgainst Paper = Scissors
winningShapeAgainst Scissors = Rock

losingShapeAgainst :: Shape -> Shape
losingShapeAgainst Rock = Scissors
losingShapeAgainst Paper = Rock
losingShapeAgainst Scissors = Paper

getRoundOutcome :: Round -> RoundOutcome
getRoundOutcome (Round (MyShape m) (OpponentShape o)) =
  RoundOutcome winner (me + MyScore (rawScore m)) (opponent + OpponentScore (rawScore o))
 where
  (winner, me, opponent) = winnerAndScores m o
  winnerAndScores Rock Scissors = win
  winnerAndScores Scissors Paper = win
  winnerAndScores Paper Rock = win
  winnerAndScores Rock Rock = draw
  winnerAndScores Paper Paper = draw
  winnerAndScores Scissors Scissors = draw
  winnerAndScores Rock Paper = loss
  winnerAndScores Paper Scissors = loss
  winnerAndScores Scissors Rock = loss
  win = (Me, MyScore 6, OpponentScore 0)
  draw = (Draw, MyScore 3, OpponentScore 3)
  loss = (Opponent, MyScore 0, OpponentScore 6)

getMyTotalScore :: [Text] -> Maybe MyScore
getMyTotalScore input = foldMap (roundOutcomeMyScore . getRoundOutcome) <$> parseMatch input

getMyPlannedTotalScore :: [Text] -> Maybe MyScore
getMyPlannedTotalScore input =
  foldMap (roundOutcomeMyScore . getRoundOutcome . roundFromPlan) <$> parsePlannedMatch input

parseMatch :: [Text] -> Maybe [Round]
parseMatch = traverse parseRound

parsePlannedMatch :: [Text] -> Maybe [RoundPlan]
parsePlannedMatch = traverse parseRoundPlan

parseRound :: Text -> Maybe Round
parseRound s = case fmap T.unpack (T.splitOn " " s) of
  [[o], [m]] -> Round <$> parseMyShape m <*> parseOpponentShape o
  _ -> Nothing

parseMyShape :: Char -> Maybe MyShape
parseMyShape 'X' = Just $ MyShape Rock
parseMyShape 'Y' = Just $ MyShape Paper
parseMyShape 'Z' = Just $ MyShape Scissors
parseMyShape _ = Nothing

parseOpponentShape :: Char -> Maybe OpponentShape
parseOpponentShape 'A' = Just $ OpponentShape Rock
parseOpponentShape 'B' = Just $ OpponentShape Paper
parseOpponentShape 'C' = Just $ OpponentShape Scissors
parseOpponentShape _ = Nothing

parseRoundPlan :: Text -> Maybe RoundPlan
parseRoundPlan s = case fmap T.unpack (T.splitOn " " s) of
  [[o], [dw]] -> RoundPlan <$> parseOpponentShape o <*> parseDesiredWinner dw
  _ -> Nothing

parseDesiredWinner :: Char -> Maybe Winner
parseDesiredWinner 'X' = Just Opponent
parseDesiredWinner 'Y' = Just Draw
parseDesiredWinner 'Z' = Just Me
parseDesiredWinner _ = Nothing
