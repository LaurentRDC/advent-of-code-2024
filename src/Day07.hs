import AOC (parseInputFileWith)
import Data.Attoparsec.Text
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE

main :: IO ()
main = do
  equations <- parseInputFileWith parseEquations

  putStrLn $ "[Part 1] " <> show (part1 equations)
  putStrLn $ "[Part 2] " <> show (part2 equations)

data Equation
  = MkEquation
  -- Some of the numbers are pretty large, so we use
  -- Integers rather than Ints to prevent silently hitting
  -- the Int upper bound
  { eqResult :: Integer,
    eqOperands :: (NonEmpty Integer)
  }
  deriving (Show)

data Operator = Add | Mul | Concat

parseEquations :: Parser [Equation]
parseEquations = (equation `sepBy` endOfLine)
  where
    equation = do
      result <- decimal <* char ':'
      skipSpace
      Just operands <- NE.nonEmpty <$> (decimal `sepBy` char ' ')
      pure $ MkEquation result operands

part1 :: [Equation] -> Integer
part1 = sum . map eqResult . filter (couldBeValid [Add, Mul])

part2 :: [Equation] -> Integer
part2 = sum . map eqResult . filter (couldBeValid [Add, Mul, Concat])

couldBeValid :: [Operator] -> Equation -> Bool
couldBeValid operatorAlphabet eq@(MkEquation _ (_ :| operands)) =
  any (valid eq) (operators (length operands))
  where
    operators :: Int -> [[Operator]]
    operators n = mapM (const operatorAlphabet) [1 .. n]

valid :: Equation -> [Operator] -> Bool
valid (MkEquation result (first :| operands)) ops =
  result
    == foldl'
      ( \acc (nextOperand, nextOperator) -> case nextOperator of
          Add -> acc + nextOperand
          Mul -> acc * nextOperand
          -- Printing the numbers and concatenating strings is dirty but it works ;)
          Concat -> read (show acc <> show nextOperand)
      )
      first
      (zip operands ops)
