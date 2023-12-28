module Day01.Day01 (solve) where

import           Text.Megaparsec
import           Utils.ParserUtils (Parser, integer)
--import Control.Monad (void)
--import Text.Megaparsec.Char (string, char, newline)

type Input = [Int]

parseInput :: Parser Input
parseInput = many integer

fuelRequired :: Int -> Int
fuelRequired m = div m 3 - 2

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print $ sum $ map fuelRequired input

fuelRequiredPt2 :: Int -> Int
fuelRequiredPt2 m
  | n <= 0 = 0
  | otherwise = n + fuelRequiredPt2 n
  where
    n = fuelRequired m

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print $ sum $ map fuelRequiredPt2 input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
