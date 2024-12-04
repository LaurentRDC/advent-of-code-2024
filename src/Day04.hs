{-# LANGUAGE OverloadedStrings #-}

import AOC (parseInputFileWith)
import Data.Attoparsec.Text
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector

main :: IO ()
main = do
  grid <- parseInputFileWith parseGrid

  let (numRows, numCols) = dimensions grid
      xmases = Set.fromList (concatMap (findXMASes grid) [(r, c) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]])

  putStrLn $ "[Part 1] Number of XMASes: " <> show (Set.size xmases)

  let mases = Set.fromList (concatMap (findMASes grid) [(r, c) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]])
      crosses = findCrosses mases

  putStrLn $ "[Part 2] Number of X-MASes: " <> show (Set.size crosses)

parseGrid :: Parser Grid
parseGrid =
  MkGrid <$> (Vector.fromList <$> many1' (row <* endOfLine))
  where
    row =
      Vector.fromList
        <$> many1'
          ( choice
              [ char 'X',
                char 'M',
                char 'A',
                char 'S'
              ]
          )

newtype Grid
  = MkGrid (Vector (Vector Char)) -- vector of rows
  deriving (Show)

dimensions :: Grid -> (Int, Int)
dimensions (MkGrid vs) =
  (Vector.length vs, Vector.length (Vector.head vs))

-- 2-dimensional indexing (row index, col index)
(!!!) :: Grid -> (Int, Int) -> Maybe Char
(!!!) grid@(MkGrid vs) (rowIndex, colIndex)
  | rowIndex < 0 || rowIndex >= maxRow = Nothing
  | colIndex < 0 || colIndex >= maxCol = Nothing
  | otherwise = Just $ (vs Vector.! rowIndex) Vector.! colIndex
  where
    (maxRow, maxCol) = dimensions grid

-- Find all the XMASes where an X is located at (rowIx, colIx)
findXMASes :: Grid -> (Int, Int) -> [XMAS]
findXMASes grid (rowIx, colIx) =
  if (grid !!! (rowIx, colIx) /= Just 'X')
    then []
    else
      mapMaybe (\(rs, cs) -> check rs cs) directions
  where
    offsets = [1, 2, 3]

    check :: [Int] -> [Int] -> Maybe XMAS
    check rowIndices colIndices =
      if all (\(rix, cix, chr) -> grid !!! (rix, cix) == Just chr) (zip3 rowIndices colIndices ['M', 'A', 'S'])
        then Just (MkXMAS (rowIx, colIx) (last rowIndices, last colIndices))
        else Nothing

    rowsLeft = [rowIx - off | off <- offsets]
    rowsRight = [rowIx + off | off <- offsets]
    rowsUnchanged = [rowIx, rowIx, rowIx]

    colsUp = [colIx - off | off <- offsets]
    colsDown = [colIx + off | off <- offsets]
    colsUnchanged = [colIx, colIx, colIx]

    directions =
      (,)
        <$> [rowsLeft, rowsRight, rowsUnchanged]
        <*> [colsUp, colsDown, colsUnchanged]

-- We can identify uniquely an 'XMAS' by the index of its extremeties
data XMAS
  = MkXMAS (Int, Int) (Int, Int)
  deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------------------
-- Part 2

-- We identify a cross by the location of its 'M's.
--
-- Use 'cross' to normalize the order of
-- M's within 'Cross'.
data MAS
  = MkMAS
      (Int, Int) -- where M is
      (Int, Int) -- where S is
  deriving (Show, Eq, Ord)

-- Location of the center 'A'
aloc :: MAS -> (Int, Int)
aloc (MkMAS (rowM, colM) (rowS, colS)) =
  ( (rowM + rowS) `quot` 2,
    (colM + colS) `quot` 2
  )

formsCross :: MAS -> MAS -> Maybe Cross
formsCross m1 m2
  | m1 == m2 = Nothing
  | aloc m1 /= aloc m2 = Nothing
  -- atan2 returns in the range [-pi, pi], so the difference
  -- can be in the [-2pi, 2pi] range.
  | (angle m1 - angle m2) `Set.member` Set.fromList [3 * pi / 2, pi / 2, -pi / 2, -3 * pi / 2] =
      Just $ MkCross (min m1 m2) (max m1 m2)
  | otherwise = Nothing
  where
    angle :: MAS -> Double
    angle mas@(MkMAS (rowM, colM) _) =
      let (rowA, colA) = aloc mas
          x = rowM - rowA
          y = colM - colA
       in atan2 (realToFrac y) (realToFrac x)

data Cross
  = MkCross MAS MAS
  deriving (Show, Eq, Ord)

-- Find all the MASes where an M is located at (rowIx, colIx)
findMASes :: Grid -> (Int, Int) -> [MAS]
findMASes grid (rowIx, colIx) =
  if (grid !!! (rowIx, colIx) /= Just 'M')
    then []
    else
      mapMaybe (\(rs, cs) -> check rs cs) directions
  where
    offsets = [1, 2]

    check :: [Int] -> [Int] -> Maybe MAS
    check rowIndices colIndices =
      if all (\(rix, cix, chr) -> grid !!! (rix, cix) == Just chr) (zip3 rowIndices colIndices ['A', 'S'])
        then Just (MkMAS (rowIx, colIx) (last rowIndices, last colIndices))
        else Nothing

    rowsLeft = [rowIx - off | off <- offsets]
    rowsRight = [rowIx + off | off <- offsets]

    colsUp = [colIx - off | off <- offsets]
    colsDown = [colIx + off | off <- offsets]

    -- ONLY diagonal MAS are allowed (no vertical / horitonzal)
    -- This messed me up
    directions = (,) <$> [rowsLeft, rowsRight] <*> [colsUp, colsDown]

findCrosses :: Set MAS -> Set Cross
findCrosses mases =
  Set.fromList $
    mapMaybe
      (uncurry formsCross)
      (Set.toList $ Set.filter (\(m1, m2) -> m1 /= m2 && aloc m1 == aloc m2) $ Set.cartesianProduct mases mases)
