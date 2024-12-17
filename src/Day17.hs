{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import AOC
import Control.Monad (guard, when)
import Control.Monad.RWS.Strict (RWS, asks, execRWS, tell)
import Data.Attoparsec.Text
import Data.Bits (xor)
import Data.List qualified as List
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)

data ProgState
  = MkProgState
  { _registerA :: Int,
    _registerB :: Int,
    _registerC :: Int,
    _instructionPtr :: Int
  }
  deriving (Show)

makeLenses ''ProgState

main :: IO ()
main = do
  (registers, program) <- parseInputFileWith (parseInput)

  putStrLn $ "[Part 1] " <> show (part1 registers program)
  putStrLn $ "[Part 2] " <> show (part2 registers program)
  where
    parseInput :: Parser ((Int, Int, Int), Program)
    parseInput = do
      regA <- string "Register A: " *> decimal <* endOfLine
      regB <- string "Register B: " *> decimal <* endOfLine
      regC <- string "Register C: " *> decimal <* endOfLine
      endOfLine
      program <- string "Program: " *> (decimal `sepBy` char ',')
      pure ((regA, regB, regC), program)

part1 :: (Int, Int, Int) -> Program -> String
part1 regs prog = List.intersperse ',' $ concatMap show $ runInterpreter regs prog

-- The trick here is to efficiently search for the appropriate value of A
-- by recognizing that the programs print A % 8.
-- Therefore, if running the program starting with registerA=X,
-- and the output is a suffix of the program, then these bits should be
-- fixed and we should search for candidates in [8*X, ..., 8*X + 7]
part2 :: (Int, Int, Int) -> Program -> Int
part2 (_, regB, regC) prog = minimum $ go 0
  where
    go :: Int -> [Int]
    go a = do
      nexta <- [a * 8 .. (a * 8) + 7]
      let u = runInterpreter (nexta, regB, regC) prog

      guard (u `List.isSuffixOf` prog)
      guard (nexta /= 0) -- prevent looping
      if u == prog
        then [nexta]
        else go nexta

type Program = [Int]

type Interpreter = RWS Program [Int] ProgState

runInterpreter :: (Int, Int, Int) -> Program -> [Int]
runInterpreter (regA, regB, regC) program =
  snd $ execRWS interpreter program (MkProgState regA regB regC 0)

interpreter :: Interpreter ()
interpreter = do
  ptr <- use instructionPtr
  asks (List.!? ptr) >>= \case
    Nothing -> pure () -- No more program
    Just opCode -> do
      operand <- asks (List.!! (ptr + 1)) -- assuming the program is well-formed; even number of elements
      runOp opCode operand
      interpreter
  where
    incPtr :: Interpreter ()
    incPtr = instructionPtr += 2

    comboOperand :: Int -> Interpreter Int
    comboOperand 0 = pure 0
    comboOperand 1 = pure 1
    comboOperand 2 = pure 2
    comboOperand 3 = pure 3
    comboOperand 4 = use registerA
    comboOperand 5 = use registerB
    comboOperand 6 = use registerC
    comboOperand o = error $ "Unexpected operand: " <> show o

    runOp :: Int -> Int -> Interpreter ()
    runOp 0 = adv
    runOp 1 = bxl
    runOp 2 = bst
    runOp 3 = jnz
    runOp 4 = bxc
    runOp 5 = out
    runOp 6 = bdv
    runOp 7 = cdv
    runOp o = error $ "Unknown opcode: " <> show o

    adv :: Int -> Interpreter ()
    adv operand = do
      co <- comboOperand operand
      registerA %= (`div` (2 ^ co))
      incPtr

    bxl :: Int -> Interpreter ()
    bxl operand = do
      registerB %= xor operand
      incPtr

    bst :: Int -> Interpreter ()
    bst operand = do
      cb <- comboOperand operand
      registerB .= cb `mod` 8
      incPtr

    jnz :: Int -> Interpreter ()
    jnz operand = do
      regA <- use registerA
      if regA /= 0
        then do
          prevInstPtr <- use instructionPtr
          instructionPtr .= operand
          when (prevInstPtr == operand) incPtr
        else do
          incPtr

    bxc :: Int -> Interpreter ()
    bxc _ = do
      regC <- use registerC
      registerB %= xor regC
      incPtr

    out :: Int -> Interpreter ()
    out operand = do
      cb <- comboOperand operand
      tell $ List.singleton (cb `mod` 8)
      incPtr

    bdv :: Int -> Interpreter ()
    bdv operand = do
      co <- comboOperand operand
      regA <- use registerA
      registerB .= regA `div` (2 ^ co)
      incPtr

    cdv :: Int -> Interpreter ()
    cdv operand = do
      co <- comboOperand operand
      regA <- use registerA
      registerC .= regA `div` (2 ^ co)
      incPtr
