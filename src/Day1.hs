{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Semigroup (Sum (..))

newtype Calories = Calories Int deriving (Eq, Show)
newtype FoodItem = FoodItem Calories deriving (Eq, Show)
newtype ElfPack = ElfPack [FoodItem] deriving (Eq, Show)

totalCalories :: ElfPack -> Int
totalCalories (ElfPack foodItems) = getSum $ foldMap getCalories foodItems
 where
  getCalories :: FoodItem -> Sum Int
  getCalories (FoodItem (Calories c)) = Sum c

getElfWithMostCaloriesTotalCalories :: [T.Text] -> Maybe Int
getElfWithMostCaloriesTotalCalories input = aux <$> parseElfPacks input
  where
    aux :: [ElfPack] -> Int
    aux eps = sum $ (take 3) $ reverse $ sort (totalCalories <$> eps)

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
