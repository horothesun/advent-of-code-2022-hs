{-# LANGUAGE DerivingVia #-}

module Day2 where

import Data.Semigroup (Sum (..))
import qualified Data.Text as T

newtype Score = Score Int
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup, Monoid) via (Sum Int)
newtype MyScore = MyScore Score
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup, Monoid) via (Sum Int)
newtype OpponentScore = OpponentScore Score
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup, Monoid) via (Sum Int)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

data Round = Round MyShape OpponentShape deriving (Eq, Show)
newtype MyShape = MyShape Shape deriving (Eq, Show)
newtype OpponentShape = OpponentShape Shape deriving (Eq, Show)

data Winner = Me | Opponent | Draw deriving (Eq, Show)

data RoundOutcome = RoundOutcome Winner MyScore OpponentScore deriving (Eq, Show)

data RoundPlan = RoundPlan OpponentShape Winner deriving (Eq, Show)

score :: Shape -> Score
score s = Score $ case s of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

roundFromPlan :: RoundPlan -> Round
roundFromPlan (RoundPlan o@(OpponentShape opponent) desiredWinner) = Round me o
 where
  me = MyShape $ case desiredWinner of
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
getRoundOutcome (Round (MyShape m) (OpponentShape o)) =
  RoundOutcome winner (MyScore $ me + score m) (OpponentScore $ opponent + score o)
 where
  win = (Me, Score 6, Score 0)
  draw = (Draw, Score 3, Score 3)
  loss = (Opponent, Score 0, Score 6)
  (winner, me, opponent) = case (m, o) of
    (Rock, Scissors) -> win
    (Scissors, Paper) -> win
    (Paper, Rock) -> win
    (Rock, Rock) -> draw
    (Paper, Paper) -> draw
    (Scissors, Scissors) -> draw
    (Rock, Paper) -> loss
    (Paper, Scissors) -> loss
    (Scissors, Rock) -> loss

getMyTotalScore :: [T.Text] -> Maybe MyScore
getMyTotalScore input = foldMap (roundOutcomeMe . getRoundOutcome) <$> parseMatch input
 where
  roundOutcomeMe (RoundOutcome _ me _) = me

getMyPlannedTotalScore :: [T.Text] -> Maybe MyScore
getMyPlannedTotalScore input =
  foldMap (roundOutcomeMe . getRoundOutcome . roundFromPlan) <$> parsePlannedMatch input
 where
  roundOutcomeMe (RoundOutcome _ me _) = me

parseMatch :: [T.Text] -> Maybe [Round]
parseMatch = traverse parseRound

parsePlannedMatch :: [T.Text] -> Maybe [RoundPlan]
parsePlannedMatch = traverse parseRoundPlan

parseRound :: T.Text -> Maybe Round
parseRound s = case fmap T.unpack (T.splitOn " " s) of
  [[o], [m]] -> Round <$> parseMyShape m <*> parseOpponentShape o
  _ -> Nothing

parseMyShape :: Char -> Maybe MyShape
parseMyShape c =
  MyShape <$> case c of
    'X' -> Just Rock
    'Y' -> Just Paper
    'Z' -> Just Scissors
    _ -> Nothing

parseOpponentShape :: Char -> Maybe OpponentShape
parseOpponentShape c =
  OpponentShape <$> case c of
    'A' -> Just Rock
    'B' -> Just Paper
    'C' -> Just Scissors
    _ -> Nothing

parseRoundPlan :: T.Text -> Maybe RoundPlan
parseRoundPlan s = case fmap T.unpack (T.splitOn " " s) of
  [[o], [dw]] -> RoundPlan <$> parseOpponentShape o <*> parseDesiredWinner dw
  _ -> Nothing

parseDesiredWinner :: Char -> Maybe Winner
parseDesiredWinner c = case c of
  'X' -> Just Opponent
  'Y' -> Just Draw
  'Z' -> Just Me
  _ -> Nothing
