{-# LANGUAGE OverloadedStrings #-}

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (getArgs)

main :: IO ()
main = do
  [inputFile] <- getArgs
  (maxrow, maxcol, antennas) <- parseInput <$> Text.readFile inputFile

  putStrLn $ "[Part 1] " <> show (part1 maxrow maxcol antennas)
  putStrLn $ "[Part 2] " <> show (part2 maxrow maxcol antennas)

newtype AntennaFrequency = MkAntennaFrequency Char
  deriving (Eq, Ord, Show)

newtype Row = MkRow Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

newtype Col = MkCol Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

type Position = (Row, Col)

parseInput :: Text -> (Row, Col, Map Position AntennaFrequency)
parseInput txt =
  let enumeratedLines =
        [ (r, c, chr)
        | (r, ls) <- zip [0 ..] (Text.lines txt),
          (c, chr) <- zip [0 ..] (Text.unpack ls)
        ]
   in foldl' go (0, 0, mempty) enumeratedLines
  where
    go :: (Row, Col, Map Position AntennaFrequency) -> (Row, Col, Char) -> (Row, Col, Map Position AntennaFrequency)
    go (maxrow, maxcol, acc) (r, c, '.') = (max maxrow r, max maxcol c, acc)
    go (maxrow, maxcol, acc) (r, c, character) =
      (max maxrow r, max maxcol c, Map.insert (r, c) (MkAntennaFrequency character) acc)

part1 :: Row -> Col -> Map Position AntennaFrequency -> Int
part1 maxRow maxCol = countAntinodes maxRow maxCol antinodes
  where
    -- Determine the two antinodes created by two antennas
    --
    -- >>> antinodes (0,0) (1,1)
    -- ((MkRow (-1),MkCol (-1)),(MkRow 2,MkCol 2))
    -- >>> antinodes (1,0) (0,3)
    -- ((MkRow 2,MkCol (-3)),(MkRow (-1),MkCol 6))
    antinodes ::
      Position ->
      Position ->
      [Position]
    antinodes (r1, c1) (r2, c2) =
      [ (r1 + (r1 - r2), c1 + (c1 - c2)),
        (r2 + (r2 - r1), c2 + (c2 - c1))
      ]

part2 :: Row -> Col -> Map Position AntennaFrequency -> Int
part2 maxRow maxCol = countAntinodes maxRow maxCol antinodesWithHarmonics
  where
    -- >>> antinodesWithHarmonics 9 9 (0,0) (1,3)
    -- [(MkRow 9,MkCol 9),(MkRow 9,MkCol 6),(MkRow 9,MkCol 3),(MkRow 8,MkCol 9),(MkRow 8,MkCol 6),(MkRow 8,MkCol 3),(MkRow 7,MkCol 9),(MkRow 7,MkCol 6),(MkRow 7,MkCol 3),(MkRow 6,MkCol 9),(MkRow 6,MkCol 6),(MkRow 6,MkCol 3),(MkRow 5,MkCol 9),(MkRow 5,MkCol 6),(MkRow 5,MkCol 3),(MkRow 4,MkCol 9),(MkRow 4,MkCol 6),(MkRow 4,MkCol 3),(MkRow 3,MkCol 9),(MkRow 3,MkCol 6),(MkRow 3,MkCol 3),(MkRow 2,MkCol 9),(MkRow 2,MkCol 6),(MkRow 2,MkCol 3),(MkRow 1,MkCol 9),(MkRow 1,MkCol 6),(MkRow 1,MkCol 3),(MkRow 0,MkCol 0),(MkRow 0,MkCol 6),(MkRow 0,MkCol 9),(MkRow 2,MkCol 0),(MkRow 2,MkCol 6),(MkRow 2,MkCol 9),(MkRow 3,MkCol 0),(MkRow 3,MkCol 6),(MkRow 3,MkCol 9),(MkRow 4,MkCol 0),(MkRow 4,MkCol 6),(MkRow 4,MkCol 9),(MkRow 5,MkCol 0),(MkRow 5,MkCol 6),(MkRow 5,MkCol 9),(MkRow 6,MkCol 0),(MkRow 6,MkCol 6),(MkRow 6,MkCol 9),(MkRow 7,MkCol 0),(MkRow 7,MkCol 6),(MkRow 7,MkCol 9),(MkRow 8,MkCol 0),(MkRow 8,MkCol 6),(MkRow 8,MkCol 9),(MkRow 9,MkCol 0),(MkRow 9,MkCol 6),(MkRow 9,MkCol 9)]
    antinodesWithHarmonics ::
      Position ->
      Position ->
      [Position]
    antinodesWithHarmonics (r1, c1) (r2, c2) =
      -- The full input is a 50x50 grid, which limits the range
      [ (row, col)
      | n <- [-50 :: Int .. 50],
        n /= 0,
        let row = r1 + fromIntegral n * (r1 - r2),
        let col = c1 + fromIntegral n * (c1 - c2),
        0 <= row && row <= maxRow,
        0 <= col && col <= maxCol
      ]
        <> [ (row, col)
           | n <- [-50 :: Int .. 50],
             n /= 0,
             let row = r2 + fromIntegral n * (r2 - r1),
             let col = c2 + fromIntegral n * (c2 - c1),
             0 <= row && row <= maxRow,
             0 <= col && col <= maxCol
           ]

countAntinodes ::
  -- | Maximum row
  Row ->
  -- | Maximum col
  Col ->
  -- | Generate antinodes from two antennas
  (Position -> Position -> [Position]) ->
  Map Position AntennaFrequency ->
  Int
countAntinodes maxRow maxCol genAntinodes antennas =
  let frequencies = Set.fromList (Map.elems antennas)
   in Set.size $ Set.unions $ Set.map go frequencies
  where
    go :: AntennaFrequency -> Set Position
    go freq =
      let antennasPos = Map.keysSet $ Map.filter (== freq) antennas
       in Set.filter (\(r, c) -> (r <= maxRow && 0 <= r && c <= maxCol && 0 <= c)) $
            Set.fromList $
              concatMap (\(x, y) -> genAntinodes x y) $
                Set.toList $
                  Set.filter (uncurry (/=)) $
                    Set.cartesianProduct antennasPos antennasPos
