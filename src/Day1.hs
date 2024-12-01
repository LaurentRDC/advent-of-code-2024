import Control.Foldl qualified as Fold
import Data.Attoparsec.Text
  ( Parser,
    decimal,
    endOfInput,
    endOfLine,
    many1',
    parseOnly,
    skipSpace,
  )
import Data.List (sort)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.IO as TIO (readFile)
import System.Environment (getArgs)

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
  Just input <- listToMaybe <$> getArgs

  Right xs <- parseOnly (parseInput <* endOfInput) <$> TIO.readFile input

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
