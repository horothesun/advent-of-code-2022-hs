{-# LANGUAGE DerivingVia #-}

module Day4 where

import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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
isOverlapping (SectionRange neL) (SectionRange neR) = null $ NE.toList neL `intersect` NE.toList neR
