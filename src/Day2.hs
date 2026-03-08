module Day2 where

import Data.Semigroup (Sum (..))
import qualified Data.Text as T

newtype Score = Score Int deriving (Eq, Show)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

data Round = Round
  { roundMe :: Shape
  , roundOpponent :: Shape
  }
  deriving (Eq, Show)

data Winner = Me | Opponent | Draw deriving (Eq, Show)

data RoundOutcome = RoundOutcome
  { roundOutcomeWinner :: Winner
  , roundOutcomeMe :: Score
  , roundOutcomeOpponent :: Score
  }
  deriving (Eq, Show)

data RoundPlan = RoundPlan
  { roundPlanOpponent :: Shape
  , roundPlanDesiredWinner :: Winner
  }
  deriving (Eq, Show)

addScores :: Score -> Score -> Score
addScores (Score l) (Score r) = Score (l + r)

scoreSum :: Score -> Sum Int
scoreSum (Score s) = Sum s

score :: Shape -> Score
score s = Score $ case s of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

roundFromPlan :: RoundPlan -> Round
roundFromPlan
  ( RoundPlan
      { roundPlanOpponent = opponent
      , roundPlanDesiredWinner = desiredWinner
      }
    ) = Round{roundMe = me, roundOpponent = opponent}
   where
    me = case desiredWinner of
      Me -> winningShapeAgainst opponent
      Opponent -> losingShapeAgainst opponent
      Draw -> opponent

winningShapeAgainst :: Shape -> Shape
winningShapeAgainst opponent = case opponent of
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

losingShapeAgainst :: Shape -> Shape
losingShapeAgainst opponent = case opponent of
  Rock -> Scissors
  Paper -> Rock
  Scissors -> Paper

getRoundOutcome :: Round -> RoundOutcome
getRoundOutcome r =
  RoundOutcome
    { roundOutcomeWinner = winner
    , roundOutcomeMe = addScores me $ score (roundMe r)
    , roundOutcomeOpponent = addScores opponent $ score (roundOpponent r)
    }
 where
  win = (Me, Score 6, Score 0)
  draw = (Draw, Score 3, Score 3)
  loss = (Opponent, Score 0, Score 6)
  (winner, me, opponent) = case (roundMe r, roundOpponent r) of
    (Rock, Scissors) -> win
    (Scissors, Paper) -> win
    (Paper, Rock) -> win
    (Rock, Rock) -> draw
    (Paper, Paper) -> draw
    (Scissors, Scissors) -> draw
    (Rock, Paper) -> loss
    (Paper, Scissors) -> loss
    (Scissors, Rock) -> loss

getMyTotalScore :: [T.Text] -> Maybe Score
getMyTotalScore input =
  (Score . getSum . foldMap scoreSum) . fmap (roundOutcomeMe . getRoundOutcome) <$> parseMatch input

getMyPlannedTotalScore :: [T.Text] -> Maybe Score
getMyPlannedTotalScore input =
  (Score . getSum . foldMap scoreSum)
    . fmap (roundOutcomeMe . getRoundOutcome . roundFromPlan)
    <$> parsePlannedMatch input

parseMatch :: [T.Text] -> Maybe [Round]
parseMatch = traverse parseRound

parsePlannedMatch :: [T.Text] -> Maybe [RoundPlan]
parsePlannedMatch = traverse parseRoundPlan

parseRound :: T.Text -> Maybe Round
parseRound s = case fmap T.unpack (T.splitOn " " s) of
  -- TODO: applicative + record notation! 🔥🔥🔥
  [[o], [m]] -> Round <$> parseMyShape m <*> parseOpponentShape o
  _ -> Nothing

parseMyShape :: Char -> Maybe Shape
parseMyShape c = case c of
  'X' -> Just Rock
  'Y' -> Just Paper
  'Z' -> Just Scissors
  _ -> Nothing

parseOpponentShape :: Char -> Maybe Shape
parseOpponentShape c = case c of
  'A' -> Just Rock
  'B' -> Just Paper
  'C' -> Just Scissors
  _ -> Nothing

parseRoundPlan :: T.Text -> Maybe RoundPlan
parseRoundPlan s = case fmap T.unpack (T.splitOn " " s) of
  -- TODO: applicative + record notation! 🔥🔥🔥
  [[o], [dw]] -> RoundPlan <$> parseOpponentShape o <*> parseDesiredWinner dw
  _ -> Nothing

parseDesiredWinner :: Char -> Maybe Winner
parseDesiredWinner c = case c of
  'X' -> Just Opponent
  'Y' -> Just Draw
  'Z' -> Just Me
  _ -> Nothing
