module FileLoader where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor

getLinesFromFile :: FilePath -> IO [T.Text]
getLinesFromFile filepath = T.readFile filepath <&> T.splitOn "\n"
