module Main (main) where

import qualified Day00.Test         as Test
import qualified Day01.Day01        as Day01
import qualified Day02.Day02        as Day02
import qualified Day03.Day03        as Day03
import qualified Day04.Day04        as Day04
import qualified Day05.Day05        as Day05
import qualified Day06.Day06        as Day06
import qualified Day07.Day07        as Day07
import qualified Day08.Day08        as Day08
import qualified Day09.Day09        as Day09
import qualified Day10.Day10        as Day10
import qualified Day11.Day11        as Day11
import qualified Day12.Day12        as Day12
import qualified Day13.Day13        as Day13
import qualified Day14.Day14        as Day14
import qualified Day15.Day15        as Day15
import qualified Day16.Day16        as Day16
import qualified Day17.Day17        as Day17
import qualified Day18.Day18        as Day18
import qualified Day19.Day19        as Day19
import qualified Day20.Day20        as Day20
import qualified Day21.Day21        as Day21
import qualified Day22.Day22        as Day22
import qualified Day23.Day23        as Day23
import qualified Day24.Day24        as Day24
import qualified Day25.Day25        as Day25

import           System.Environment (getArgs)

solvers :: [FilePath -> IO ()]
solvers =
    [ Test.solve
    , Day01.solve
    , Day02.solve
    , Day03.solve
    , Day04.solve
    , Day05.solve
    , Day06.solve
    , Day07.solve
    , Day08.solve
    , Day09.solve
    , Day10.solve
    , Day11.solve
    , Day12.solve
    , Day13.solve
    , Day14.solve
    , Day15.solve
    , Day16.solve
    , Day17.solve
    , Day18.solve
    , Day19.solve
    , Day20.solve
    , Day21.solve
    , Day22.solve
    , Day23.solve
    , Day24.solve
    , Day25.solve
    ]

main :: IO ()
main = do
    [day, filePath] <- getArgs
    let solver = solvers !! (read day)
    putStrLn ""
    solver filePath
