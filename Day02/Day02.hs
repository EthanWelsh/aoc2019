module Day02.Day02 (solve) where

import           Control.Lens         (element, (.~))
import           Data.Tuple.Extra     (uncurry3)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, newline)
import           Utils.ParserUtils    (Parser, integer)

newtype Memory = Memory [Int] deriving (Show)
newtype Address = Address Int deriving (Show, Num)
data Instruction = Add Address Address Address
                 | Multiply Address Address Address
                 | End deriving (Show)
type Input = Memory

instructionSize :: Instruction -> Int
instructionSize (Add {})      = 4
instructionSize (Multiply {}) = 4
instructionSize End           = 1

parseInput :: Parser Input
parseInput = do
   m <- integer `sepBy` char ','
   return $ Memory m

get :: Memory -> Address -> Int
get m a = m @ a

set :: Memory -> Address -> Int -> Memory
set (Memory m) (Address a) v = Memory $ (element a .~ v) m

setAll :: Memory -> [(Address, Int)] -> Memory
setAll = foldl (\m (a, v) -> set m a v)

(@) :: Memory -> Address -> Int
(@) = get

(@+) :: Address -> Int -> Address
(@+) (Address a) offset = Address $ a + offset

runInstruction :: Memory -> Instruction -> Memory
runInstruction m End              = m
runInstruction m (Add a b r)      = set m r (m @ a + m @ b)
runInstruction m (Multiply a b r) = set m r (m @ a * m @ b)

setInputs :: Memory -> (Int, Int) -> Memory
setInputs m (verb, noun) = setAll m [(1, verb), (2, noun)]

next3 :: Memory -> Address -> (Address, Address, Address)
next3 m a = (Address $ m @ (a @+ 1), Address $ m @ (a @+ 2), Address $ m @ (a @+ 3))

instructionAtAddress :: Memory -> Address -> Instruction
instructionAtAddress m a = let
  opcode = m @ a
  in case opcode of
    1  -> uncurry3 Add (next3 m a)
    2  -> uncurry3 Multiply (next3 m a)
    99 -> End

isEnd :: Instruction -> Bool
isEnd End = True
isEnd _   = False

runProgram :: Memory -> Address -> Memory
runProgram m a = let
  instruction = instructionAtAddress m a
  nextA = a @+ instructionSize instruction
  result = runInstruction m instruction
  in if isEnd instruction
     then m
     else runProgram result nextA

possibleInputs :: [(Int, Int)]
possibleInputs = [(a, b) | a <- [0..99], b <- [0..99]]


runProgramWithInputs :: Memory -> (Int, Int) -> Int
runProgramWithInputs m inputs = let
  mm = setInputs m inputs
  in runProgram mm (Address 0) @ 0

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let instructions = setInputs input (12, 2)
  print $ runProgramWithInputs input (12, 2)

findRightInputs :: Memory -> Int -> [(Int, Int)] -> (Int, Int)
findRightInputs m target (x:xs) = let
  result = runProgramWithInputs m x
  in if result == target then x else findRightInputs m target xs

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let (n, v) = findRightInputs input 19690720 possibleInputs
  print $ 100 * n + v

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
