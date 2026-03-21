{-# LANGUAGE DerivingVia #-}

module Day4 where

import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

newtype SectionId = SectionId Int deriving (Eq, Show)

newtype SectionRange = SectionRange (NonEmpty SectionId) deriving (Eq, Show)

data AssignmentPair = AssignmentPair SectionRange SectionRange deriving (Eq, Show)

isFullyContainedIn :: SectionRange -> SectionRange -> Bool
isFullyContainedIn (SectionRange neL) (SectionRange neR) = ls == ls `intersect` rs
 where
  ls = NE.toList neL
  rs = NE.toList neR

isOverlapping :: SectionRange -> SectionRange -> Bool
isOverlapping (SectionRange neL) (SectionRange neR) = [] /= (NE.toList neL `intersect` NE.toList neR)

isOneRangeFullyContainedIntoTheOther :: AssignmentPair -> Bool
isOneRangeFullyContainedIntoTheOther (AssignmentPair l r) = (l `isFullyContainedIn` r) || (r `isFullyContainedIn` l)

areRangesOverlapping :: AssignmentPair -> Bool
areRangesOverlapping (AssignmentPair l r) = l `isOverlapping` r

getAssignmentPairsWithFullyContainedRangeCount :: [Text] -> Maybe Int
getAssignmentPairsWithFullyContainedRangeCount input =
  assignmentPairsWithFullyContainedRangeCount <$> parseAssignmentPairs input

assignmentPairsWithFullyContainedRangeCount :: [AssignmentPair] -> Int
assignmentPairsWithFullyContainedRangeCount = length . filter isOneRangeFullyContainedIntoTheOther

getAssignmentPairsWithOverlappingRangesCount :: [Text] -> Maybe Int
getAssignmentPairsWithOverlappingRangesCount input =
  assignmentPairsWithOverlappingRangesCount <$> parseAssignmentPairs input

assignmentPairsWithOverlappingRangesCount :: [AssignmentPair] -> Int
assignmentPairsWithOverlappingRangesCount = length . filter areRangesOverlapping

parseAssignmentPairs :: [Text] -> Maybe [AssignmentPair]
parseAssignmentPairs = traverse parseAssignmentPair

parseAssignmentPair :: Text -> Maybe AssignmentPair
parseAssignmentPair t = case T.splitOn "," t of
  [l, r] -> AssignmentPair <$> parseSectionRange l <*> parseSectionRange r
  _ -> Nothing

parseSectionRange :: Text -> Maybe SectionRange
parseSectionRange t = case T.splitOn "-" t of
  [minStr, maxStr] -> case (textToMaybeInt minStr, textToMaybeInt maxStr) of
    (Just minInt, Just maxInt) -> Just . SectionRange $ SectionId <$> minInt :| [minInt + 1 .. maxInt]
    _ -> Nothing
  _ -> Nothing

textToMaybeInt :: Text -> Maybe Int
textToMaybeInt t = case TR.decimal t of
  Right (n, rest)
    | T.null rest -> Just n
    | otherwise -> Nothing
  Left _ -> Nothing
