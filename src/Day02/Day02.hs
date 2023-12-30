module Day02.Day02 (solve) where

import           Text.Megaparsec
import           Text.Megaparsec.Char (char)
import           Utils.Intcode
import           Utils.ParserUtils    (Parser, integer)

type Input = Machine

parseInput :: Parser Input
parseInput = do
   m <- integer `sepBy` char ','
   return $ machineFromList m

possibleInputs :: [(Int, Int)]
possibleInputs = [(a, b) | a <- [0..99], b <- [0..99]]

setInputs :: Memory -> (Int, Int) -> Memory
setInputs m (verb, noun) = setAll m [(Address 1, verb), (Address 2, noun)]

runProgramWithInputs :: Machine -> (Int, Int) -> Int
runProgramWithInputs m inputs = let
  mem = getMemory m
  m' = m { getMemory = setInputs mem inputs }
  in getMemory (runProgram m') @ Address 0

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print $ runProgramWithInputs input (12, 2)

findRightInputs :: Machine -> Int -> [(Int, Int)] -> (Int, Int)
findRightInputs _ _ [] = error "Reached end of inputs before finding a match"
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
