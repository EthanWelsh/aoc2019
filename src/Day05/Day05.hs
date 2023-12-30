module Day05.Day05 (solve) where

import           Text.Megaparsec hiding (getInput)
import           Text.Megaparsec.Char (char, newline, string)
import           Utils.Intcode
import           Utils.ParserUtils    (Parser, signedInteger)

type Input = Machine

parseInput :: Parser Input
parseInput = do
   m <- signedInteger `sepBy` char ','
   return $ machineFromList m

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let m = input { getInput = [1] }
  print $ getOutput $ runProgram $ m

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: " 
  --let m = input { getInput = [1] }
  --print $ instructionAtAddress (getMemory m) (Address 6)
  --print input
  -- 3 + 1100

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
