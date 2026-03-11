{-# LANGUAGE DerivingVia #-}

module Day3 where

import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

newtype Item = Item Char deriving (Eq, Show)

newtype Priority = Priority Int deriving (Eq, Show, Num) via Int

data Rucksack = Rucksack
  { rucksackFirstHalf :: NonEmpty Item
  , rucksackSecondHalf :: NonEmpty Item
  }
  deriving (Eq, Show)

allItems :: Rucksack -> NonEmpty Item
allItems r = rucksackFirstHalf r `NE.append` rucksackSecondHalf r

repeatedItemsBetweenCompartments :: Rucksack -> [Item]
repeatedItemsBetweenCompartments r = firstHalf `intersect` secondHalf
 where
  firstHalf = NE.toList $ rucksackFirstHalf r
  secondHalf = NE.toList $ rucksackSecondHalf r

lowercaseChars :: [Char]
lowercaseChars = ['a' .. 'z']

uppercaseChars :: [Char]
uppercaseChars = ['A' .. 'Z']

priority :: Item -> Priority
priority (Item c) =
  Priority $
    if c `elem` lowercaseChars
      then
        code - 96
      else
        if c `elem` uppercaseChars
          then
            code - 38
          else 0
 where
  code = fromEnum c

getRepeatedItemsPrioritiesSum :: [Text] -> Maybe Priority
getRepeatedItemsPrioritiesSum = fmap repeatedItemsPrioritiesSum . parseRucksacks

repeatedItemsPrioritiesSum :: [Rucksack] -> Priority
repeatedItemsPrioritiesSum rs = sum (priority <$> mapMaybe (listToMaybe . repeatedItemsBetweenCompartments) rs)

parseItem :: Char -> Maybe Item
parseItem c =
  if c `elem` (lowercaseChars ++ uppercaseChars)
    then
      Just (Item c)
    else Nothing

parseRucksack :: Text -> Maybe Rucksack
parseRucksack s =
  if odd (T.length s)
    then Nothing
    else case (ls, rs) of
      (lh : lt, rh : rt) ->
        let firstHalf = traverse parseItem (lh :| lt)
            secondHalf = traverse parseItem (rh :| rt)
         in Rucksack <$> firstHalf <*> secondHalf
      _ -> Nothing
 where
  (l, r) = T.splitAt (T.length s `div` 2) s
  ls = T.unpack l
  rs = T.unpack r

parseRucksacks :: [Text] -> Maybe [Rucksack]
parseRucksacks = traverse parseRucksack
