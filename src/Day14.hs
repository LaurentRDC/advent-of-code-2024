{-# LANGUAGE OverloadedStrings #-}

import AOC (parseInputFileWith)
import Control.Foldl qualified as Fold
import Data.Attoparsec.Text
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

main :: IO ()
main = do
  robots <- parseInputFileWith (parseRobot `sepBy` (endOfLine))

  putStrLn $ "[Part 1] " <> show (part1 101 103 robots)
  putStrLn $ "[Part 2] " <> show (part2 robots)
  where
    parseRobot :: Parser Robot
    parseRobot =
      MkRobot
        <$> (string "p=" *> pair <* char ' ')
        <*> (string "v=" *> pair)
      where
        pair =
          (,)
            <$> signed decimal
            <* char ','
            <*> signed decimal

part1 :: Row -> Col -> [Robot] -> Int
part1 nrows ncols robots =
  let after100s = fmap (advance 100 nrows ncols) robots
   in product $ Map.elems $ Map.filterWithKey (\k _ -> isJust k) $ Fold.fold (Fold.groupBy quadrant Fold.length) after100s
  where
    horzCenter = (nrows - 1) `div` 2
    vertCenter = (ncols - 1) `div` 2

    quadrant :: Robot -> Maybe Quadrant
    quadrant (MkRobot (r, c) _)
      | r < horzCenter && c < vertCenter = Just TopLeft
      | r < horzCenter && c > vertCenter = Just TopRight
      | r > horzCenter && c < vertCenter = Just BotLeft
      | r > horzCenter && c > vertCenter = Just BotRight
      | otherwise = Nothing

data Quadrant
  = TopLeft
  | TopRight
  | BotLeft
  | BotRight
  deriving (Eq, Ord, Show)

-- For part 2, I assume that the sum of the distance between any two robots
-- will be minimized. Therefore, we track the sum of the distance between
-- robot 1 and 2, robot 2 and 3, etc.
--
-- Since the movement of the robots follows modular arithmetic, we need to check at most 101 * 103 steps.
part2 :: [Robot] -> Int
part2 robots = 1 + (fst $ Vector.minimumOn snd $ Vector.indexed $ Vector.unfoldrExactN (101 * 103) go (0, Vector.fromList robots))
  where
    go :: (Int, Vector Robot) -> (Int, (Int, Vector Robot))
    go (secondsElapsed, robs) =
      let robots' = fmap (advance 1 101 103) robs
       in (Vector.sum (Vector.zipWith dist robots' (cycle_ robots')), (succ secondsElapsed, robots'))

    dist :: Robot -> Robot -> Int
    dist (MkRobot (x1, y1) _) (MkRobot (x2, y2) _) = fromIntegral (abs (x1 - x2)) + fromIntegral (abs (y1 - y2))

    cycle_ :: Vector Robot -> Vector Robot
    cycle_ vs
      | Vector.null vs = Vector.empty
      | otherwise = Vector.tail vs <> Vector.singleton (Vector.head vs)

newtype Row = MkRow Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

newtype Col = MkCol Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

type Position = (Row, Col)

data Robot
  = MkRobot
  { position :: Position,
    velocity :: Position
  }
  deriving (Show)

advance :: Int -> Row -> Col -> Robot -> Robot
advance nSteps numRows numCols (MkRobot (x, y) (vx, vy)) =
  MkRobot ((x + (fromIntegral nSteps) * vx) `mod` numRows, (y + (fromIntegral nSteps) * vy) `mod` numCols) (vx, vy)
