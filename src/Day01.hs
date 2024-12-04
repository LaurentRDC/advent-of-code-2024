import AOC (parseInputFileWith)
import Control.Foldl qualified as Fold
import Data.Attoparsec.Text
  ( Parser,
    decimal,
    endOfLine,
    many1',
    skipSpace,
  )
import Data.List (sort)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)

parseInput :: Parser [(Int, Int)]
parseInput = do
  many1' $ do
    left <- decimal
    skipSpace
    right <- decimal
    endOfLine
    pure (left, right)

main :: IO ()
main = do
  xs <- parseInputFileWith parseInput

  let (leftsUnsorted, rightsUnsorted) = unzip xs
      lefts = sort leftsUnsorted
      rights = sort rightsUnsorted

      totalDistance = sum (zipWith (\l r -> abs $ l - r) rights lefts)

  putStrLn $ "[Part 1] Total distance: " <> show totalDistance

  let rightCounts = runCounter rightsUnsorted
      similarity = sum [num * fromMaybe 0 (Map.lookup num rightCounts) | num <- leftsUnsorted]

  putStrLn $ "[Part 2] Similarity: " <> show similarity

type Occurences = Int

type Counter = Map Int Occurences

runCounter :: [Int] -> Counter
runCounter = Fold.fold (Fold.groupBy id ((Fold.premap (const 1) Fold.sum)))
