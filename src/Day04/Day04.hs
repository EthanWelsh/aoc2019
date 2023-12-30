module Day04.Day04 (solve) where

import           Control.Monad        (void)
import           Data.List            (group)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char)
import           Utils.ParserUtils    (Parser, integer)

type Input = (Int, Int)

-- The value is within the range given in your puzzle input.

pairs :: [a] -> [(a, a)]
pairs []           = []
pairs [_]          = []
pairs (x : y : ys) = (x, y) : pairs (y : ys)

-- It is a six-digit number.
isSixDigits :: Int -> Bool
isSixDigits n = 6 == length (show n)

-- Two adjacent digits are the same (like 22 in 122345).
hasTwoSameAdjacentDigits :: Int -> Bool
hasTwoSameAdjacentDigits n = any (uncurry (==)) ((pairs . show) n)

-- Going from left to right, the digits never decrease; they only ever increase
-- or stay the same (like 111123 or 135679).
digitsNeverDecrease :: Int -> Bool
digitsNeverDecrease n = not $ any (uncurry (>)) ((pairs . show) n)

parseInput :: Parser Input
parseInput = do
  a <- integer
  void $ char '-'
  b <- integer
  return (a, b)

allp :: [a -> Bool] -> a -> Bool
allp ps a = and (map ($ a) ps)

part1 :: Input -> IO ()
part1 (a, b) = do
  putStr "Part 1: "
  let predicates = [isSixDigits, hasTwoSameAdjacentDigits, digitsNeverDecrease]
  let possibles = [a .. b]
  let filtered = filter (allp predicates) possibles
  print $ length filtered

-- the two adjacent matching digits are not part of a larger group of matching
-- digits.
noLargeGroups :: Int -> Bool
noLargeGroups n = any ((== 2) . length) (group (show n))

part2 :: Input -> IO ()
part2 (a, b) = do
  putStr "Part 2: "
  let predicates = [isSixDigits, hasTwoSameAdjacentDigits, digitsNeverDecrease, noLargeGroups]
  let possibles = [a .. b]
  let filtered = filter (allp predicates) possibles
  print $ length filtered

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
