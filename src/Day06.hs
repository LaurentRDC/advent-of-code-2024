{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (second)
import Data.List (unfoldr)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (getArgs)

main :: IO ()
main = do
  [inputFile] <- getArgs
  (Just guardStartingPosition, obstacles) <- parseInput <$> Text.readFile inputFile

  putStrLn $ "[Part 1] " <> show (part1 guardStartingPosition obstacles)
  putStrLn $ "[Part 2] " <> show (part2 guardStartingPosition obstacles)

newtype Obstacles
  = MkObstacles (Set Position)

newtype Row = MkRow Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

newtype Col = MkCol Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

parseInput :: Text -> (Maybe Position, Obstacles)
parseInput txt =
  let enumeratedLines =
        [ (r, c, chr)
        | (r, ls) <- zip [0 ..] (Text.lines txt),
          (c, chr) <- zip [0 ..] (Text.unpack ls)
        ]
   in second (MkObstacles) $ foldl' go (Nothing, Set.empty) enumeratedLines
  where
    go :: (Maybe Position, Set Position) -> (Row, Col, Char) -> (Maybe Position, Set Position)
    go (guardPos, obstacles) (r, c, '#') = (guardPos, MkPosition (r, c) `Set.insert` obstacles)
    go (Nothing, obstacles) (r, c, '^') = (Just (MkPosition (r, c)), obstacles)
    go (Just _, _) (_, _, '^') = error "This should never happen"
    go acc _ = acc

newtype Position = MkPosition (Row, Col) deriving (Show, Eq, Ord)

newtype Direction = MkDirection (Row, Col) deriving (Show, Eq, Ord)

turnRight :: Direction -> Direction
turnRight (MkDirection (0, 1)) = MkDirection (1, 0)
turnRight (MkDirection (1, 0)) = MkDirection (0, -1)
turnRight (MkDirection (0, -1)) = MkDirection (-1, 0)
turnRight (MkDirection (-1, 0)) = MkDirection (0, 1)
turnRight _ = error "This should never happen"

part1 :: Position -> Obstacles -> Int
part1 guardStartPos obstacles =
  Set.size $ guardStartPos `Set.insert` path guardStartPos obstacles
  where

path :: Position -> Obstacles -> Set Position
path guardStartPos (MkObstacles obstacles )
  = Set.fromList $ unfoldr advance (guardStartPos, MkDirection (-1, 0))
  where
    maxRow = Set.findMax $ Set.map (\(MkPosition (r, _)) -> r) obstacles
    maxCol = Set.findMax $ Set.map (\(MkPosition (_, c)) -> c) obstacles

    move :: Position -> Direction -> Position
    move (MkPosition (a, b)) (MkDirection (x, y)) = MkPosition (a + x, b + y)

    advance :: (Position, Direction) -> Maybe (Position, (Position, Direction))
    advance (pos, dir)
      | nextPosition `Set.member` obstacles = Just (pos, (pos, turnRight dir))
      | nextRow < 0 || nextRow > maxRow = Nothing
      | nextCol < 0 || nextCol > maxCol = Nothing
      | otherwise = Just (nextPosition, (nextPosition, dir))
      where
        nextPosition@(MkPosition (nextRow, nextCol)) = move pos dir

part2 :: Position -> Obstacles -> Int
part2 guardStartPos (MkObstacles obstacles) =
  length $
    filter hasLoop $
      [ MkObstacles (pos `Set.insert` obstacles)
      -- We only evaluate new obstacles where the guard
      -- would have walked
      | pos <- Set.toList (path guardStartPos (MkObstacles obstacles))
      ]
  where
    maxRow = Set.findMax $ Set.map (\(MkPosition (r, _)) -> r) obstacles
    maxCol = Set.findMax $ Set.map (\(MkPosition (_, c)) -> c) obstacles

    -- This could easily be parallelized; currently it is rather slow
    hasLoop :: Obstacles -> Bool
    hasLoop (MkObstacles obs) = go (guardStartPos, MkDirection (-1, 0)) Set.empty
      where
        go :: (Position, Direction) -> Set (Position, Direction) -> Bool
        go (pos, dir) pathHistory
          -- Path will loop because we've been in this situation before
          | (pos, dir) `Set.member` pathHistory = True
          -- leaving the lab
          | nextRow < 0 || nextRow > maxRow = False
          | nextCol < 0 || nextCol > maxCol = False
          -- Turning due to obstacle
          | nextPosition `Set.member` obs = go (pos, turnRight dir) ((pos, dir) `Set.insert` pathHistory)
          -- Advancing
          | otherwise = go (nextPosition, dir) ((pos, dir) `Set.insert` pathHistory)
          where
            nextPosition@(MkPosition (nextRow, nextCol)) = move pos dir

    move :: Position -> Direction -> Position
    move (MkPosition (a, b)) (MkDirection (x, y)) = MkPosition (a + x, b + y)
