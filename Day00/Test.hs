-- Day 0: Test

module Day00.Test (solve) where

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  putStrLn contents
