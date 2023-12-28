module Day03.Day03 (solve) where

import           Data.Function        (on)
import           Data.List            (minimumBy)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, newline, string)
import           Utils.Maze
import           Utils.ParserUtils    (Parser, integer)

type Step = (Direction, Int)
type Line = [Step]
type Input = (Line, Line)

parseStep :: Parser Step
parseStep = do
  d <- choice [ North <$ char 'U'
              , East <$ char 'R'
              , South <$ char 'D'
              , West <$ char 'L']
  v <- integer
  return (d, v)

parseLine :: Parser Line
parseLine = parseStep `sepBy` char ','

parseInput :: Parser Input
parseInput = do
  a <- parseLine
  b <- parseLine
  return (a, b)

step :: Point -> Step -> [Point]
step p (d, 0) = []
step p (d, c) = let
  newPoint = movePoint p d
  futurePoints = step newPoint (d, c - 1)
  in newPoint : futurePoints

pointsInLine :: Point -> Line -> [Point]
pointsInLine current [] = [current]
pointsInLine current (s:ss) = let
  pointsFromCurrentSegment = step current s
  lastPointInSegment = last pointsFromCurrentSegment
  in  pointsFromCurrentSegment ++ (pointsInLine lastPointInSegment ss)

getCrossingPoints :: [Point] -> [Point] -> [Point]
getCrossingPoints a b = S.toList $ S.intersection (S.fromList a) (S.fromList b)

part1 :: Input -> IO ()
part1 (a, b) = do
  putStr "Part 1: "
  let origin = (0, 0)
  let aPoints = pointsInLine origin a
  let bPoints = pointsInLine origin b
  let crossingPoints = getCrossingPoints aPoints bPoints
  let distances = map (manhattanDistance origin) crossingPoints
  print $ minimum distances

distanceToPoint :: [Point] -> Int -> M.Map Point Int
distanceToPoint [] _     = M.empty
distanceToPoint (p:ps) d = M.union (M.singleton p d) (distanceToPoint ps (d+1))

part2 :: Input -> IO ()
part2 (a, b) = do
  putStr "Part 2: "
  let origin = (0, 0)
  let aPoints = pointsInLine origin a
  let bPoints = pointsInLine origin b
  let aDistances = distanceToPoint aPoints 1
  let bDistances = distanceToPoint bPoints 1
  let crossingPoints = getCrossingPoints aPoints bPoints
  let distances = map (\p -> aDistances M.! p + bDistances M.! p) crossingPoints
  print $ minimum distances

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
