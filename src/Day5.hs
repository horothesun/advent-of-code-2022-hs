{-# LANGUAGE DerivingVia #-}

module Day5 where

import Control.Applicative (many, (<|>))
import Control.Monad.Combinators.NonEmpty (some)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Stack
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec (MonadParsec (try), Parsec, anySingle, between, runParser, skipManyTill)
import Text.Megaparsec.Char (char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Witherable (Filterable (mapMaybe))

newtype Crate = Crate Char deriving (Eq, Show)

newtype Stacks = Stacks (NonEmpty (Stack Crate)) deriving (Eq, Show)

newtype From = From Natural deriving (Eq, Show, Num) via Natural
newtype To = To Natural deriving (Eq, Show, Num) via Natural

data Move = Move Natural From To deriving (Eq, Show)

data BaseMove = BaseMove From To deriving (Eq, Show)

newtype BaseMovePlan = BaseMovePlan [BaseMove] deriving (Eq, Show)

newtype MovePlan = MovePlan [Move] deriving (Eq, Show)

data CrateItem
  = NoCrate
  | CrateItem Crate
  deriving (Eq, Show)

peekAll :: Stacks -> NonEmpty (Maybe Crate)
peekAll (Stacks ss) = stackPeek <$> ss

emptyStacks :: Natural -> Maybe Stacks
emptyStacks 0 = Nothing
emptyStacks n = Just $ Stacks $ stackNew :| (stackNew <$ [2 .. n])

noCrateShow :: Text
noCrateShow = "[~]"

preProcessInput :: [Text] -> [Text]
preProcessInput = fmap preProcessInputRow

preProcessInputRow :: Text -> Text
preProcessInputRow = T.drop 1 . T.replace "    " (" " `T.append` noCrateShow) . T.append " "

stacksFromCrateItemGrid :: NonEmpty (NonEmpty CrateItem) -> Stacks
stacksFromCrateItemGrid rows = Stacks $ stackFromList . mapMaybe maybeCrate . NE.toList <$> NE.transpose rows

maybeCrate :: CrateItem -> Maybe Crate
maybeCrate NoCrate = Nothing
maybeCrate (CrateItem c) = Just c

-- parsers

type Parser = Parsec Void Text
type Parsingrror = ParseErrorBundle Text Void

crateP :: Parser Crate
crateP = Crate <$> between (char '[') (char ']') letterChar

crateItemP :: Parser CrateItem
crateItemP = (NoCrate <$ string noCrateShow) <|> (CrateItem <$> crateP)

crateItemGridP :: Parser (NonEmpty (NonEmpty CrateItem))
crateItemGridP = some $ some crateItemP

stacksP :: Parser Stacks
stacksP = stacksFromCrateItemGrid <$> crateItemGridP

-- parser :: Parser [Instruction]
-- parser = many (try (skipManyTill anySingle (try operationParser)))

-- parseAll :: T.Text -> Either ParsingError [Instruction]
-- parseAll = runParser parser "input file"
