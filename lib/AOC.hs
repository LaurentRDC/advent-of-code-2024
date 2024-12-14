{-# LANGUAGE LambdaCase #-}

module AOC
  ( parseInputFileWith,
    Row (getRow),
    row,
    Col (getCol),
    col,
  )
where

import Data.Attoparsec.Text
  ( Parser,
    endOfInput,
    parseOnly,
  )
import Data.Maybe (listToMaybe)
import Data.Text.IO as TIO (readFile)
import System.Environment (getArgs)

-- | Parse an input file whose filepath is provided as
-- the first command-line argument, and return the result.
--
-- The parser will be applied to the entirety of the input.
-- Parse errors will result in exceptions.
parseInputFileWith :: Parser a -> IO a
parseInputFileWith parser = do
  listToMaybe <$> getArgs >>= \case
    Nothing -> error "No input file specified."
    Just input -> do
      parseOnly (parser <* endOfInput) <$> TIO.readFile input
        >>= \case
          Left errmsg -> error errmsg
          Right result -> pure result

newtype Row = MkRow {getRow :: Int}
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

row :: Int -> Row
row = MkRow

newtype Col = MkCol {getCol :: Int}
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

col :: Int -> Col
col = MkCol
