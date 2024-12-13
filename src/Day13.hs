{-# LANGUAGE OverloadedStrings #-}

import AOC (parseInputFileWith)
import Data.Attoparsec.Text
import Numeric.LinearAlgebra (Matrix, (><))
import Numeric.LinearAlgebra qualified as Lin

main :: IO ()
main = do
  machines <- parseInputFileWith (parseClawMachine `sepBy` (endOfLine))

  putStrLn $ "[Part 1] " <> show (part1 machines)
  putStrLn $ "[Part 2] " <> show (part2 machines)
  where
    parseClawMachine :: Parser ClawMachine
    parseClawMachine = do
      buttonA <-
        (,)
          <$> (string "Button A: X+" *> decimal <* string ", Y+")
          <*> (decimal <* endOfLine)

      buttonB <-
        (,)
          <$> (string "Button B: X+" *> decimal <* string ", Y+")
          <*> (decimal <* endOfLine)
      prize <-
        (,)
          <$> (string "Prize: X=" *> decimal <* string ", Y=")
          <*> (decimal <* endOfLine)
      pure $ MkClawMachine buttonA buttonB prize

part1 :: [ClawMachine] -> Int
part1 = sum . fmap numTokens

part2 :: [ClawMachine] -> Int
part2 = sum . fmap (numTokens . shift)
  where
    shift :: ClawMachine -> ClawMachine
    shift (MkClawMachine butA butB (xP, yP)) =
      MkClawMachine butA butB (xP + 10000000000000, yP + 10000000000000)

data ClawMachine
  = -- I tested using `Integer` rather than `Int`, but numbers
  -- aren't large enough to make a difference, even in Part 2
  MkClawMachine
  { buttonA :: (Int, Int),
    buttonB :: (Int, Int),
    price :: (Int, Int)
  }
  deriving (Show)

-- Change of basis from (X, Y) to (A, B)
numTokens :: ClawMachine -> Int
numTokens (MkClawMachine (xA, yA) (xB, yB) (xP, yP)) =
  case Lin.linearSolve
    ((2 >< 2) (realToFrac <$> [xA, xB, yA, yB]) :: Matrix Double)
    ((2 >< 1) [realToFrac xP, realToFrac yP]) of
    Just sol ->
      let a = round $ sol `Lin.atIndex` (0, 0)
          b = round $ sol `Lin.atIndex` (1, 0)
       in if (a * xA + b * xB == xP) && (a * yA + b * yB == yP)
            then 3 * a + b
            else 0 -- a and b aren't integers
    Nothing -> 0
