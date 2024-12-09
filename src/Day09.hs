import AOC (parseInputFileWith)
import Data.Attoparsec.Text hiding (take)
import Data.Bifunctor (first)
import Data.Char (digitToInt)
import Data.List (intersperse)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Semigroup (Arg (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector

main :: IO ()
main = do
  (memLayout, fileSizes) <- parseInputFileWith parseMemoryLayout

  putStrLn $ "[Part 1] " <> show (part1 memLayout)
  putStrLn $ "[Part 2] " <> show (part2 fileSizes memLayout)

type FileID = Int

type MemoryLayout = Vector (Maybe FileID)

parseMemoryLayout :: Parser (MemoryLayout, Map FileID Int)
parseMemoryLayout = do
  digits <- many1' (digitToInt <$> digit)
  let fileIds = intersperse Nothing $ map Just [0 ..]
      fileSizes = Map.fromList $ map (first fromJust) $ filter (isJust . fst) $ (zip fileIds digits)
      layout = Vector.fromList $ concatMap (uncurry replicate) (zip digits fileIds)
  pure (layout, fileSizes)

part1 :: MemoryLayout -> Int
part1 vs = checksum $ vs Vector.// (part1Updates vs)

part2 :: Map FileID Int -> MemoryLayout -> Int
part2 fileSizes vs = checksum (part2Updates fileSizes vs)

checksum :: MemoryLayout -> Int
checksum = sum . Vector.map (\(ix, mfileid) -> maybe 0 (* ix) mfileid) . Vector.indexed

part1Updates :: MemoryLayout -> [(Int, Maybe FileID)]
part1Updates vs = go [] 0 (Vector.length vs - 1)
  where
    -- We traverse the vector `vs` once, with two pointers: one starting from
    -- the left, and one starting from the right
    go :: [(Int, Maybe FileID)] -> Int -> Int -> [(Int, Maybe FileID)]
    go acc leftPtr rightPtr
      | leftPtr > rightPtr = acc -- done
      | otherwise =
          let left = vs ! leftPtr
              right = vs ! rightPtr
           in case (left, right) of
                (Nothing, Just _) -> go (acc <> [(leftPtr, right), (rightPtr, left)]) (leftPtr + 1) (rightPtr - 1)
                (Nothing, Nothing) -> go acc leftPtr (rightPtr - 1)
                (Just _, Nothing) -> go acc (leftPtr + 1) (rightPtr - 1)
                (Just _, Just _) -> go acc (leftPtr + 1) rightPtr

part2Updates :: Map FileID Int -> MemoryLayout -> MemoryLayout
part2Updates fileSizes vs = go vs Set.empty (Vector.length vs - 1)
  where
    -- It would be much faster to run the following
    -- computation using mutable vectors in the ST monad.
    go :: MemoryLayout -> Set FileID -> Int -> MemoryLayout
    go layout visitedFiles rightPtr
      | rightPtr <= 0 = layout
      | otherwise = case layout ! rightPtr of
          Nothing -> go layout visitedFiles (rightPtr - 1)
          Just fid ->
            if fid `Set.member` visitedFiles
              then go layout visitedFiles (rightPtr - 1)
              else
                let fsize = fileSizes Map.! fid
                    availableSpaces =
                      filter (\mem -> width mem >= fsize) $
                        emptyMemory $
                          (Vector.slice 0 rightPtr layout)
                 in case availableSpaces of
                      [] -> go layout (Set.insert fid visitedFiles) (rightPtr - 1)
                      ((MkEmptyMemory start end) : _) ->
                        let (filestart, fileend) = (rightPtr - fsize + 1, rightPtr)
                            fileMoveUpdate = [(ix, Just fid) | ix <- take fsize [start .. end]]
                            emptyMemUpdate = [(ix, Nothing) | ix <- [filestart .. fileend]]
                         in go (layout Vector.// (fileMoveUpdate <> emptyMemUpdate)) (Set.insert fid visitedFiles) (rightPtr - 1)

    -- >>> emptyMemory $ Vector.fromList [ Just 0, Just 0, Nothing, Nothing, Nothing, Just 1, Just 1, Nothing, Just 2 ]
    -- [MkEmptyMemory 2 4,MkEmptyMemory 7 7]
    --
    -- This function is defined within `part2Updates` because
    -- it relies on the structure of the vector, most importantly that
    -- file IDs are un-fragmented
    emptyMemory :: MemoryLayout -> [EmptyMemory]
    emptyMemory ys =
      let groups = Vector.group $ Vector.map (uncurry Arg . swap) $ Vector.indexed ys
       in map (\xs -> MkEmptyMemory (Vector.minimum xs) (Vector.maximum xs)) $
            map (Vector.map (\(Arg _ ix) -> ix)) $
              filter (Vector.any (\(Arg mfileId _) -> isNothing mfileId)) groups

data EmptyMemory
  = MkEmptyMemory Int Int
  deriving (Show)

width :: EmptyMemory -> Int
width (MkEmptyMemory start end) = 1 + (end - start)
