import AOC
import Algorithm.Search (dijkstra)
import Data.Attoparsec.Text hiding (take)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as Set

main :: IO ()
main = do
  fallSchedule <- parseInputFileWith parseInput

  let gridSize = (70, 70)
      numBytesFallen = 1024

  putStrLn $ "[Part 1] " <> show (part1 gridSize numBytesFallen fallSchedule)
  putStrLn $ "[Part 2] " <> show (part2 gridSize numBytesFallen fallSchedule)
  where
    parseInput :: Parser (IntMap Position)
    parseInput = do
      rcs <- parsePair `sepBy` endOfLine
      pure $ IntMap.fromList $ zip [0 ..] rcs
      where
        parsePair =
          (,)
            <$> (decimal <* char ',')
            <*> decimal

type Position = (Row, Col)

part1 :: (Row, Col) -> Int -> IntMap Position -> Int
part1 (maxRow, maxCol) numFallen fallSchedule =
  case dijkstra neighbors (\_ _ -> 1) (== (maxRow, maxCol)) (0, 0) of
    Nothing -> error "oh no"
    Just (numSteps, _) -> numSteps
  where
    corruptedTiles = Set.fromList $ map snd $ take numFallen $ IntMap.toAscList fallSchedule
    validTiles = (Set.fromList $ (,) <$> [0 .. maxRow] <*> [0 .. maxCol]) `Set.difference` corruptedTiles

    neighbors (r, c) = Set.fromList [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)] `Set.intersection` validTiles

-- This would definitely be faster as a fold
part2 :: (Row, Col) -> Int -> IntMap Position -> Position
part2 (maxRow, maxCol) numFallen fallSchedule =
  fromJust $ do
    (ix, _) <- List.uncons (dropWhile stillPathToExit [1 ..])
    IntMap.lookup (numFallen + ix) fallSchedule
  where
    stillPathToExit :: Int -> Bool
    stillPathToExit n = isJust (dijkstra neighbors (\_ _ -> 1 :: Int) (== (maxRow, maxCol)) (0, 0))
      where
        corruptedTiles = Set.fromList $ map snd $ take (numFallen + n + 1) $ IntMap.toAscList fallSchedule
        validTiles = (Set.fromList $ (,) <$> [0 .. maxRow] <*> [0 .. maxCol]) `Set.difference` corruptedTiles

        neighbors (r, c) = Set.fromList [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)] `Set.intersection` validTiles
