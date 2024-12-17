import AOC
import Algorithm.Search (dijkstra)
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Tree (Tree (..))
import Data.Tree qualified as Tree
import System.Environment (getArgs)

main :: IO ()
main = do
  [inputFile] <- getArgs
  (start, end, paths) <- parsePaths <$> Text.readFile inputFile

  putStrLn $ "[Part 1] " <> show (part1 start end paths)
  putStrLn $ "[Part 2] " <> show (part2 start end paths)
  where
    parsePaths :: Text -> (Position, Position, Set Position)
    parsePaths txt =
      ( head $
          [ (r, c)
          | (r, ls) <- zip [0 ..] (Text.lines txt),
            (c, chr) <- zip [0 ..] (Text.unpack ls),
            chr == 'S'
          ],
        head $
          [ (r, c)
          | (r, ls) <- zip [0 ..] (Text.lines txt),
            (c, chr) <- zip [0 ..] (Text.unpack ls),
            chr == 'E'
          ],
        Set.fromList
          [ (r, c)
          | (r, ls) <- zip [0 ..] (Text.lines txt),
            (c, chr) <- zip [0 ..] (Text.unpack ls),
            chr /= '#'
          ]
      )

type Position = (Row, Col)

type Direction = (Row, Col)

part1 :: Position -> Position -> Set Position -> Int
part1 start end paths =
  let solution =
        dijkstra
          neighbors
          score
          (\(p, _) -> p == end)
          (start, (0, 1))
   in case solution of
        Nothing -> error "oh no"
        Just (s, _) -> s
  where
    neighbors :: (Position, Direction) -> Set (Position, Direction)
    neighbors ((r, c), (dr, dc)) =
      Set.filter (\(p, _) -> p `Set.member` paths) $
        Set.fromList
          [ ((r + dr, c + dc), (dr, dc)),
            ((r, c), (1, 0)),
            ((r, c), (-1, 0)),
            ((r, c), (0, 1)),
            ((r, c), (0, -1))
          ]

    score :: (Position, Direction) -> (Position, Direction) -> Int
    score (pos1, dir1) (pos2, dir2)
      -- Direction is the same -> 1 point for walking forward
      | dir1 == dir2 = 1
      | pos1 == pos2 = 1000
      | otherwise = error "no score"

-- This is very slow, but I don't have time today
part2 :: Position -> Position -> Set Position -> Int
part2 start end nodes 
  = Set.size $ Set.unions $ (Set.fromList . NonEmpty.toList) <$> allShortests
  where
    allNodes = (nodes <> Set.fromList [start, end])
    tree =
      Tree.unfoldTree
        ( \(p, dir, visited) ->
            let ns = neighbors (p, dir) `Set.difference` visited -- no backtracking by following visited nodes
             in (p, [(n, ndir, visited <> Set.singleton (n, ndir)) | (n, ndir) <- Set.toList ns])
        )
        (start, (0, 1), Set.empty)

    neighbors :: (Position, Direction) -> Set (Position, Direction)
    neighbors ((r, c), (dr, dc))
      | (r, c) == end = Set.empty
      | otherwise =
          Set.filter (\(_, (newr, newc)) -> notFlipping (dr, dc) (newr, newc)) $
            Set.fromList
              [ (newpos, (dr_, dc_))
              | (dr_, dc_) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
                notFlipping (dr, dc) (dr_, dc_), -- only <90deg turns
                let newpos = (r + dr_, c + dc_),
                newpos `Set.member` allNodes
              ]
      where
        notFlipping :: Direction -> Direction -> Bool
        notFlipping (1, 0) (-1, 0) = False
        notFlipping (-1, 0) (1, 0) = False
        notFlipping (0, 1) (0, -1) = False
        notFlipping (0, -1) (0, 1) = False
        notFlipping _ _ = True

    paths = NonEmpty.filter (\path -> NonEmpty.head path == end) (treePaths tree)

    shortest = length $ List.minimumBy (compare `on` length) (NonEmpty.toList <$> paths)
    allShortests = filter ((== shortest) . length) paths

treePaths :: Tree a -> NonEmpty (NonEmpty a)
treePaths = NonEmpty.fromList . go []
  where
    -- Even though this actually returns a non-empty list, returning
    -- a normal list is easier for the recursive case where we concatMap
    -- the results from 'go'.
    go :: [a] -> Tree a -> [NonEmpty a]
    go history (Node focus forest) =
      ((focus :| history) : List.concatMap (go (focus : history)) forest)
