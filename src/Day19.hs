{-# LANGUAGE OverloadedStrings #-}

import AOC
import Algorithm.Search (dfs)
import Data.Attoparsec.Text as Parser
import Data.List qualified as List
import Data.Maybe (isJust, mapMaybe)
import Data.MemoTrie (memo)
import Data.Text qualified as Text

main :: IO ()
main = do
  (availablePatterns, desiredPatterns) <- parseInputFileWith parseInput

  putStrLn $ "[Part 1] " <> show (part1 availablePatterns desiredPatterns)
  putStrLn $ "[Part 2] " <> show (part2 availablePatterns desiredPatterns)
  where
    parseInput :: Parser ([String], [String])
    parseInput = do
      avail <- (Parser.takeWhile1 (`elem` ['w', 'u', 'b', 'r', 'g']) `sepBy` string ", ") <* endOfLine
      endOfLine
      patterns <- (Parser.takeWhile1 (`elem` ['w', 'u', 'b', 'r', 'g'])) `sepBy` endOfLine
      pure
        ( map Text.unpack avail,
          map Text.unpack patterns
        )

type Pattern = String

part1 :: [Pattern] -> [Pattern] -> Int
part1 availablePatterns desiredPatterns =
  length $ filter (isJust . search) desiredPatterns
  where
    search :: Pattern -> Maybe [Pattern]
    search p =
      dfs
        ( \state ->
            [ chunk
            | stripes <- availablePatterns,
              let chunk = state ++ stripes,
              chunk `List.isPrefixOf` p
            ]
        )
        (== p)
        mempty

part2 :: [Pattern] -> [Pattern] -> Int
part2 availablePatterns desiredPatterns =
  sum (numArrangements <$> desiredPatterns)
  where
    -- We use memoization because since the alphabet is rather restricting,
    -- we will be counting over many similar subsequences.
    numArrangements :: Pattern -> Int
    numArrangements =
      memo $
        \p ->
          if null p -- pattern entirely consumed
            then 1
            else
              sum
                ( map
                    numArrangements
                    (mapMaybe (\towel -> (towel `List.stripPrefix` p)) availablePatterns)
                )
