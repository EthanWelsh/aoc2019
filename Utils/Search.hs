module Utils.Search (
  hasPath,
  allPaths,
) where

import qualified Data.Set as S

hasPath :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> Bool
hasPath neighbors isEnd n = hasPathHelper neighbors isEnd S.empty n

hasPathHelper :: Ord a => (a -> [a]) -> (a -> Bool) -> S.Set a -> a -> Bool
hasPathHelper neighbors isEnd visited n
  | isEnd n = True
  | n `S.member` visited = False
  | otherwise = let
    newVisited = S.insert n visited
    in any (hasPathHelper neighbors isEnd newVisited) (neighbors n)

allPaths :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> [[a]]
allPaths neighbors isEnd start = allPathsHelper neighbors isEnd [] start

allPathsHelper :: Ord a => (a -> [a]) -> (a -> Bool) -> [a] -> a -> [[a]]
allPathsHelper neighbors isEnd path n
  | isEnd n = [path]
  | n `elem` path = []
  | otherwise = concatMap (allPathsHelper neighbors isEnd (n:path)) (neighbors n)
