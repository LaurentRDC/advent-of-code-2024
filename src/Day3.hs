{-# LANGUAGE OverloadedStrings #-}

import AOC (parseInputFileWith)
import Control.Applicative (many)
import Control.Monad (void)
import Data.Attoparsec.Text
import Data.Either (lefts)

main :: IO ()
main = do
  xs <- parseInputFileWith parseMemory

  putStrLn $ "[Part 1] Sum of valid multiplications: " <> show (sum $ [left * right | Mul left right <- xs])
  putStrLn $ "[Part 2] Sum of valid multiplications: " <> show (interpret xs)

data Instruction
  = Mul Int Int
  | Do
  | Dont
  deriving (Show)

parseMemory :: Parser [Instruction]
parseMemory = lefts <$> many (eitherP validInstruction anyChar)
  where
    validInstruction =
      choice
        [ string "do()" >> pure Do,
          string "don't()" >> pure Dont,
          do
            void $ string "mul("
            left <- decimal
            void $ char ','
            right <- decimal
            void $ char ')'
            pure $ Mul left right
        ]

data MulState
  = MulEnabled
  | MulDisabled

data State
  = MkState MulState !Int

interpret :: [Instruction] -> Int
interpret =
  (\(MkState _ result) -> result)
    . foldl' go (MkState MulEnabled 0)
  where
    go (MkState MulEnabled accumulator) (Mul left right) = MkState MulEnabled (accumulator + left * right)
    go (MkState MulDisabled acc) (Mul _ _) = MkState MulDisabled acc
    go (MkState _ acc) Do = MkState MulEnabled acc
    go (MkState _ acc) Dont = MkState MulDisabled acc
