{-# LANGUAGE TypeFamilies #-}

import AOC (parseInputFileWith)
import Data.Attoparsec.Text hiding (take)
import Data.Coerce (coerce)
import Data.MemoTrie (HasTrie (..), Reg, enumerateGeneric, memo, trieGeneric, untrieGeneric, (:->:))
import GHC.Generics (Generic)

main :: IO ()
main = do
  rocks <- parseInputFileWith ((MkRockLabel <$> decimal) `sepBy` char ' ')

  putStrLn $ "[Part 1] " <> show (part1 rocks)
  putStrLn $ "[Part 2] " <> show (part2 rocks)

part1 :: [RockLabel] -> Int
part1 = sum . map (rocksAfterBlinking 25)

part2 :: [RockLabel] -> Int
part2 = sum . map (rocksAfterBlinking 75)

rocksAfterBlinking :: Step -> RockLabel -> Int
rocksAfterBlinking 0 = const 1 -- If no more blinks left, rocks can't multiply
rocksAfterBlinking nstep =
  -- I'm going to be honest, I don't understand how `memo` works, but
  -- it makes this function ultra fast
  -- More information about memo:
  --    http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries
  memo $ sum . map (rocksAfterBlinking (nstep - 1)) . blink
  where
    blink :: RockLabel -> [RockLabel]
    blink i
      | i == 0 = [1]
      | even (length (digits (coerce i))) =
          let (left, right) = splitHalf (coerce i)
           in [MkRockLabel left, MkRockLabel right]
      | otherwise = [2024 * i]

newtype Step = MkStep Int
  deriving (Eq, Ord, Show, Num)

newtype RockLabel = MkRockLabel Int
  deriving (Eq, Ord, Show, Num, Generic)

instance HasTrie RockLabel where
  newtype RockLabel :->: b = RockLabelTrie (Reg RockLabel :->: b)
  trie = trieGeneric RockLabelTrie
  untrie = untrieGeneric coerce
  enumerate = enumerateGeneric coerce

-- >>> splitHalf 1000
-- (10,0)
splitHalf :: Int -> (Int, Int)
splitHalf n =
  let dgts = digits n
      halfLen = length dgts `div` 2
   in (undigits (take halfLen dgts), undigits (drop halfLen dgts))

-- The two following functions were adapted from the `digits` package
-- and specialized to base 10
digits :: Int -> [Int]
digits 0 = []
digits x =
  let (rest, lastDigit) = quotRem x 10
   in digits rest <> [lastDigit]

undigits :: [Int] -> Int
undigits = foldl (\a b -> a * 10 + b) 0
