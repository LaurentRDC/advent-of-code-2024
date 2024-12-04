import AOC (parseInputFileWith)
import Data.Attoparsec.Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector

main :: IO ()
main = do
  xs <- parseInputFileWith parseReports

  putStrLn $ "[Part 1] Number of safe reports: " <> show (length (filter isReportSafe xs))
  putStrLn $ "[Part 2] Number of safe reports with dampener: " <> show (length (filter isReportSafeWithDampener xs))

parseReports :: Parser [Report]
parseReports = (Vector.fromList <$> decimal `sepBy1` char ' ') `sepBy1` (satisfy isEndOfLine) <* endOfLine

type Report = Vector Int

isReportSafe :: Report -> Bool
isReportSafe vs =
  let diff = Vector.zipWith (-) vs (Vector.tail vs) -- Elementwise difference
   in isMonotonic diff && isDiffCapped diff
  where
    isMonotonic x = (Vector.all (< 0) x) || (Vector.all (> 0) x)

    isDiffCapped x = Vector.foldl' isSafe True x
    isSafe False _ = False
    isSafe True x = x /= 0 && abs x <= 3

isReportSafeWithDampener :: Report -> Bool
isReportSafeWithDampener vs =
  -- The simplest way is to brute-force. No need or time to optimize.
  --
  -- Another way to do this would be to accumulate a 'Map Int Bool' 
  -- result, rather than simply 'Bool', where the result at index I
  -- ignored the input at index I. This way, each report could be
  -- traversed only once.
  
  -- Note: the initial (-1) index isn't in `vs`, so the first
  --       element of the list is `vs` unchanged.
  any isReportSafe $ [deleteAt ix vs | ix <- [-1 .. length vs - 1]]
  where
    deleteAt :: Int -> Vector Int -> Vector Int
    deleteAt ix = Vector.ifilter (\i _ -> i /= ix)
