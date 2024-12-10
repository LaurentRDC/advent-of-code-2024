{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import System.Environment (getArgs)

main :: IO ()
main = do
  [inputFile] <- getArgs
  topomap <- parseInput <$> Text.readFile inputFile

  putStrLn $ "[Part 1] " <> show (part1 topomap)

  putStrLn $ "[Part 2] " <> show (part2 topomap)

part1 :: TopoMap -> Int
part1 topomap =
  sum $
    map (Set.size . Set.fromList . leaves . pathsToNine topomap . fst) $
      Map.toList $
        Map.filter (== 0) topomap

part2 :: TopoMap -> Int
part2 topomap =
  sum $
    map (length . leaves . pathsToNine topomap . fst) $
      Map.toList $
        Map.filter (== 0) topomap
  where

leaves :: Tree Position -> [Position]
leaves = last . Tree.levels

pathsToNine :: TopoMap -> Position -> Tree Position
pathsToNine topomap start = Tree.unfoldTree go start
  where
    go :: Position -> (Position, [Position])
    go current
      | Map.lookup current topomap == Just 9 = (current, [])
      | otherwise =
          let heights =
                filter (\(_, h) -> h == height + 1) $
                  catMaybes [(k,) <$> Map.lookup k topomap | k <- neighbors current]
           in (current, map fst heights)
      where
        Just height = Map.lookup current topomap

        neighbors :: Position -> [Position]
        neighbors (r, c) =
          [ (r - 1, c),
            (r + 1, c),
            (r, c - 1),
            (r, c + 1)
          ]

newtype Row = MkRow Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

newtype Col = MkCol Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

type Position = (Row, Col)

type TopoMap = Map Position Int

parseInput :: Text -> TopoMap
parseInput txt =
  Map.fromList
    [ ((r, c), digitToInt height)
    | (r, ls) <- zip [0 ..] (Text.lines txt),
      (c, height) <- zip [0 ..] (Text.unpack ls)
    ]
