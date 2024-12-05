{-# LANGUAGE OverloadedStrings #-}

import AOC (parseInputFileWith)
import Data.Attoparsec.Text
import Data.List (sortBy, tails)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
  (orders, updates) <- parseInputFileWith parseInput

  putStrLn $ "[Part 1] " <> show (part1 orders updates)
  putStrLn $ "[Part 2] " <> show (part2 orders updates)

data Order
  = MkOrder Int Int
  deriving (Show, Eq, Ord)

newtype Update
  = MkUpdate [Int]
  deriving (Show)

middle :: Update -> Int
middle (MkUpdate xs)
  | even (length xs) = error "Should never happen"
  | otherwise = xs !! (length xs `div` 2)

parseInput :: Parser (Set Order, [Update])
parseInput =
  (,)
    <$> (Set.fromList <$> (order `sepBy1'` endOfLine) <* (endOfLine >> endOfLine))
    <*> (update `sepBy1'` endOfLine)
  where
    order =
      MkOrder
        <$> (decimal <* char '|')
        <*> decimal

    update = MkUpdate <$> decimal `sepBy1` char ','

alreadyInOrder :: Set Order -> Update -> Bool
alreadyInOrder orders (MkUpdate xs) =
  let relations = pairs xs
   in all id (map (\(first, second) -> MkOrder second first `Set.notMember` orders) relations) -- checking for violations

-- Generate pairs such that the first element of the pair always
-- occurs first
--
-- >>> pairs [1,2,3,4]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
pairs :: [a] -> [(a, a)]
pairs xs = concat $ mapMaybe go (tails xs)
  where
    go [] = Nothing
    go [_] = Nothing
    go (h : rest) = Just $ (h,) <$> rest

part1 :: Set Order -> [Update] -> Int
part1 orders updates =
  sum $ map middle (filter (alreadyInOrder orders) updates)

part2 :: Set Order -> [Update] -> Int
part2 orders updates =
  let outOfOrder = filter (not . alreadyInOrder orders) updates
   in sum [middle (ordered u) | u <- outOfOrder]
  where
    ordered :: Update -> Update
    ordered (MkUpdate xs) =
      MkUpdate (sortBy f xs)
      where
        -- Someone on Reddit noticed that the ordering rules are complete, not sparse.
        -- This means that we can have a comparison function like below which
        -- is complete.
        f left right = if MkOrder left right `Set.member` orders then LT else GT
