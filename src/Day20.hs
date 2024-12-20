import AOC
import Algorithm.Search (bfs)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (getArgs)

main :: IO ()
main = do
  [inputFile] <- getArgs
  (start, end, walls) <- parsePaths <$> Text.readFile inputFile

  putStrLn $ "[Part 1] " <> show (part1 100 start end walls)
  putStrLn $ "[Part 2] " <> show (part2 100 start end walls)
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
            chr == '#'
          ]
      )

type Position = (Row, Col)

type Savings = Int

part1 :: Int -> Position -> Position -> Set Position -> Int
part1 minSavings start end walls =
  case bfs
        (\pos -> Set.fromList (neighbors pos) `Set.difference` walls)
        (== end)
        start of
    Nothing -> error "impossible!"
    Just vanillaPath ->
      let augmented = augmentPath vanillaPath
        in length
            [ (i1, i2)
            | activation <- vanillaPath,
              deactivation <- vanillaPath,
              activation /= deactivation,
              let d = dist activation deactivation,
              d == 2,
              let i1 = augmented Map.! activation,
              let i2 = augmented Map.! deactivation,
              (i2 - i1) >= minSavings + d
            ]

data Cheat
  = MkCheat !Position -- activation site
            !Position -- deactivation site
  deriving (Eq, Ord)

-- Map from position along the best path to
-- how many steps are required until the end
augmentPath :: [Position] -> Map Position Int
augmentPath ps =
  Map.fromList $
    zip
      ps
      (reverse [1 .. length ps])

cheatSavings ::
  Map Position Int ->
  Cheat ->
  Savings
cheatSavings augmentedPath (MkCheat activation deactivation) =
  case augmentedPath Map.!? deactivation of
    Nothing -> 0
    Just nextLeftSteps ->
      let prevLeftSteps = augmentedPath Map.! activation
        in prevLeftSteps - nextLeftSteps - dist deactivation activation -- che

neighbors :: Position -> [Position]
neighbors (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

dist :: Position -> Position -> Int
dist (r1, c1) (r2, c2) = fromIntegral (abs (r1 - r2)) + fromIntegral (abs (c1 - c2))

part2 :: Int -> Position -> Position -> Set Position -> Int
part2 minSavings start end walls =
  case bfs
        (\pos -> Set.fromList (neighbors pos) `Set.difference` walls)
        (== end)
        start of
    Nothing -> error "impossible!"
    Just vanillaPath ->
      length $
        filter (>= minSavings) $  
            [ (cheatSavings augmented cheat)
            | let augmented = augmentPath vanillaPath,
              activation <- vanillaPath,
              deactivation <- vanillaPath,
              activation /= deactivation,
              activation `dist` deactivation <= 20,
              let cheat = (MkCheat activation deactivation)
            ]
