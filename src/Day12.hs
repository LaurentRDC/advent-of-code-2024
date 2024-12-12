import Control.Exception (assert)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Semigroup (sconcat)
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (getArgs)

main :: IO ()
main = do
  [inputFile] <- getArgs
  regs <- regions <$> parseInput <$> Text.readFile inputFile

  -- let sds = Seq.filter (\(c, _) -> c == 'A') $ fmap (\reg@(MkRegion c _) -> (c, sides $ fences reg)) regs
  -- -- let sds = fmap (length . sides . fences) regs
  -- print ( sds)

  putStrLn $ "[Part 1] " <> show (part1 regs)
  putStrLn $ "[Part 2] " <> show (part2 regs)
  where
    parseInput :: Text -> Garden
    parseInput txt =
      Map.fromList
        [ ((r, c), height)
        | (r, ls) <- zip [0 ..] (Text.lines txt),
          (c, height) <- zip [0 ..] (Text.unpack ls)
        ]

part1 :: Seq Region -> Int
part1 = sum . fmap (\r -> area r * perimeter r)

part2 :: Seq Region -> Int
part2 = sum . fmap (\r -> area r * (sides r))

area :: Region -> Int
area = Set.size . plots

perimeter :: Region -> Int
perimeter (MkRegion _ xs) = sum $ map (\p -> 4 - numTouching p) (Set.toAscList xs)
  where
    numTouching pos = Set.size (Set.filter (\p2 -> pos `distFrom` p2 == 1) xs)

newtype Row = MkRow Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

newtype Col = MkCol Int
  deriving (Show, Eq, Ord, Enum, Bounded, Real, Num, Integral)

type Position = (Row, Col)

distFrom :: Position -> Position -> Int
distFrom (r1, c1) (r2, c2) =
  abs (fromIntegral (r1 - r2)) + abs (fromIntegral (c1 - c2))

type Garden = Map Position Char

data Region
  = MkRegion
  { plantType :: Char,
    plots :: Set Position
  }
  deriving (Eq, Ord, Show)

instance Semigroup Region where
  (<>) (MkRegion c1 xs1) (MkRegion c2 xs2) =
    assert (c1 == c2) $ MkRegion c1 (xs1 <> xs2)

isAdjacent :: Position -> Char -> Region -> Bool
isAdjacent p1 c1 (MkRegion c2 plots) = c1 == c2 && any (\p2 -> p1 `distFrom` p2 == 1) plots

regions :: Garden -> Seq Region
regions = Map.foldlWithKey go Seq.empty
  where
    go :: Seq Region -> Position -> Char -> Seq Region
    go acc pos plant
      | Seq.null acc = Seq.singleton (MkRegion plant (Set.singleton pos))
      | otherwise =
          let (adjacent, notAdjacent) = Seq.partition (isAdjacent pos plant) acc
           in notAdjacent >< Seq.singleton (sconcat (MkRegion plant (Set.singleton pos) :| (Foldable.toList adjacent)))

-- This function comes from the following observation: we can check if
-- a position is at a corner of the shape by peering into the directions
-- of the corner (for example, peeing into the up + left direction) and
-- checking if the position there is inside our current region or not
--
-- >>> walls $ Set.fromList [(0,0), (0, 1), (1,0), (1,1)]
-- 4
sides :: Region -> Int
sides (MkRegion _ xs) =
  sum
    [ countBy (isCorner horz vert) xs
    | horz <- [left, right],
      vert <- [up, down]
    ]
  where
    countBy p = length . Set.filter p

    up, down, left, right :: Position -> Position
    up (r, c) = (r - 1, c)
    down (r, c) = (r + 1, c)
    left (r, c) = (r, c - 1)
    right (r, c) = (r, c + 1)

    isCorner ::
      (Position -> Position) ->
      (Position -> Position) ->
      Position ->
      Bool
    isCorner dir1 dir2 x =
      isEdge dir1 && (isEdge dir2 || not isCross)
      where
        isEdge :: (Position -> Position) -> Bool
        isEdge dir = dir x `Set.notMember` xs

        -- Handling cases of the form:
        -- A B
        -- B A
        isCross = (isEdge (dir1 . dir2))
