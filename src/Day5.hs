{-# LANGUAGE DerivingVia #-}

module Day5 where

import Control.Applicative (many, (<|>))
import qualified Control.Monad as Parser
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Stack
import Data.Text (Text)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec (Parsec, between, eof, runParser, sepBy, sepBy1)
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Witherable (Filterable (mapMaybe))

newtype Crate = Crate Char deriving (Eq, Show)

newtype Stacks = Stacks (NonEmpty (Stack Crate)) deriving (Eq, Show)

newtype Count = Count Natural deriving (Eq, Show, Num) via Natural
newtype From = From Natural deriving (Eq, Show, Num) via Natural
newtype To = To Natural deriving (Eq, Show, Num) via Natural

data Move = Move !Count !From !To deriving (Eq, Show)

data BaseMove = BaseMove !From !To deriving (Eq, Show)

newtype MovePlan = MovePlan [Move] deriving (Eq, Show)

newtype BaseMovePlan = BaseMovePlan [BaseMove] deriving (Eq, Show)

data CrateItem
  = NoCrate
  | CrateItem !Crate
  deriving (Eq, Show)

peekAll :: Stacks -> NonEmpty (Maybe Crate)
peekAll (Stacks ss) = stackPeek <$> ss

emptyStacks :: Natural -> Maybe Stacks
emptyStacks 0 = Nothing
emptyStacks n = Just $ Stacks $ stackNew :| (stackNew <$ [2 .. n])

stacksFromCrateItemGrid :: NonEmpty (NonEmpty CrateItem) -> Stacks
stacksFromCrateItemGrid rows = Stacks $ stackFromList . mapMaybe maybeCrate . NE.toList <$> NE.transpose rows

maybeCrate :: CrateItem -> Maybe Crate
maybeCrate NoCrate = Nothing
maybeCrate (CrateItem c) = Just c

-- parsers

type Parser = Parsec Void Text
type ParsingError = ParseErrorBundle Text Void

sepBy1NE :: Parser a -> Parser b -> Parser (NonEmpty a)
sepBy1NE p sep = do
  as <- p `sepBy1` sep
  case NE.nonEmpty as of
    Nothing -> Parser.fail "Expected non-empty list"
    Just ne -> pure ne

someNE :: Parser a -> Parser (NonEmpty a)
someNE p = (:|) <$> p <*> many p

parseAll :: Text -> Either ParsingError (Stacks, MovePlan)
parseAll = runParser allP ""

allP :: Parser (Stacks, MovePlan)
allP = do
  stacks <- stacksP
  _ <- newline
  movePlan <- movePlanP
  _ <- eof
  return (stacks, movePlan)

stacksP :: Parser Stacks
stacksP = stacksFromCrateItemGrid <$> crateItemGridP

crateItemGridP :: Parser (NonEmpty (NonEmpty CrateItem))
crateItemGridP = someNE ((crateItemP `sepBy1NE` char ' ') <* newline) <* indexesP

crateItemP :: Parser CrateItem
crateItemP = (NoCrate <$ string "   ") <|> (CrateItem <$> crateP)

crateP :: Parser Crate
crateP = Crate <$> between (char '[') (char ']') letterChar

indexesP :: Parser ()
indexesP = do
  _ <- char ' ' *> digitChar `sepBy1` string "   " *> char ' ' *> newline
  return ()

movePlanP :: Parser MovePlan
movePlanP = MovePlan <$> (moveP `sepBy` newline)

moveP :: Parser Move
moveP = Move <$> (string "move " *> countP) <*> (string " from " *> fromP) <*> (string " to " *> toP)

countP :: Parser Count
countP = Count <$> decimal

fromP :: Parser From
fromP = From <$> decimal

toP :: Parser To
toP = To <$> decimal
