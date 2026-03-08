{-# LANGUAGE DerivingVia #-}

module Day1 where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Ord (Down (Down), comparing)
import Data.Semigroup (Sum (..))
import qualified Data.Text as T
import qualified Data.Text.Read as TR

newtype Calories = Calories Int
  deriving (Eq, Show, Num, Ord) via Int
  deriving (Semigroup, Monoid) via (Sum Int)

newtype FoodItem = FoodItem Calories deriving (Eq, Show)
newtype ElfPack = ElfPack [FoodItem] deriving (Eq, Show)

foodItemCalories :: FoodItem -> Calories
foodItemCalories (FoodItem c) = c

totalCalories :: ElfPack -> Calories
totalCalories (ElfPack foodItems) = foldMap foodItemCalories foodItems

getElfWithMostCaloriesTotalCalories :: [T.Text] -> Maybe Calories
getElfWithMostCaloriesTotalCalories input = maximum . fmap totalCalories <$> parseElfPacks input

getTop3ElvesWithMostCaloriesTotalCalories :: [T.Text] -> Maybe Calories
getTop3ElvesWithMostCaloriesTotalCalories input = aux <$> parseElfPacks input
 where
  aux :: [ElfPack] -> Calories
  aux = sum . take 3 . sortBy (comparing Data.Ord.Down) . fmap totalCalories

parseFoodItem :: T.Text -> Maybe FoodItem
parseFoodItem t = case TR.decimal t of
  Right (n, rest)
    | T.null rest -> Just $ FoodItem $ Calories n
    | otherwise -> Nothing
  Left _ -> Nothing

parseElfPack :: [T.Text] -> Maybe ElfPack
parseElfPack items = ElfPack <$> traverse parseFoodItem items

parseElfPacks :: [T.Text] -> Maybe [ElfPack]
parseElfPacks input = traverse parseElfPack $ splitOn [""] input
