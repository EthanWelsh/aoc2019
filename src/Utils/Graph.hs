module Utils.Graph (
  Graph,
  edges,
  graphFromEdges,
  graphAsMap,
  graphFromMap,
  apply,
  addEdge,
  removeEdge,
  removeBidirectionalEdge,
  neighbors,
  nodes,
  reachable,
  connectedComponents,
  makeBidirectional
) where

import           Data.List       ((\\))
import           Data.List.Extra (groupSort)
import           Data.Map        as M (Map, adjust, insertWith, keys,
                                       fromList, toList, (!))
import           Data.Set        (Set, empty, insert, member,
                                  notMember, unions)
import Data.Tuple (swap)

newtype Graph a = Graph (Map a [a]) deriving (Show)

makeBidirectional :: Ord a => Graph a -> Graph a
makeBidirectional g = let
  allEdges = (edges g) ++ map swap (edges g)
  in graphFromEdges allEdges

flatten :: (a, [b]) -> [(a, b)]
flatten (a, bs) = map (a,) bs

unflatten :: Ord a => [(a, b)] -> [(a, [b])]
unflatten = groupSort

edges :: Graph a -> [(a, a)]
edges g = concatMap flatten (M.toList (graphAsMap g))

graphFromEdges :: Ord a => [(a, a)] -> Graph a
graphFromEdges es = graphFromMap $ M.fromList $ unflatten es

apply :: (Map a [a] -> Map a [a]) -> Graph a -> Graph a
apply f (Graph g) = Graph $ f g

graphAsMap :: Graph a -> Map a [a]
graphAsMap (Graph m) = m

graphFromMap :: Map a [a] -> Graph a
graphFromMap a = Graph a

addEdge :: Ord a => Graph a -> a -> a -> Graph a
addEdge g src dst = apply (M.insertWith (++) src [dst]) g

removeEdge :: Ord a => Graph a -> (a, a) -> Graph a
removeEdge g (src, dst) = apply (adjust (\es -> es \\ [dst]) src) g

removeBidirectionalEdge :: Ord a => Graph a -> (a, a) -> Graph a
removeBidirectionalEdge g e = let
  g1 = removeEdge g e
  g2 = removeEdge g1 (swap e)
  in g2

neighbors :: Ord a => Graph a -> a -> [a]
neighbors (Graph g) n = g ! n

nodes :: Graph a -> [a]
nodes (Graph g) = keys g

reachable :: Ord a => Graph a -> a -> Set a
reachable g n = helper g empty n
  where
    helper gg visited nn
      | nn `member` visited = visited
      | otherwise = let
        newVisited = insert nn visited
        ns = neighbors gg nn
        in unions $ map (helper gg newVisited) ns

connectedComponents :: Ord a => Graph a -> [Set a]
connectedComponents g = connectedHelper g (nodes g)
  where
    connectedHelper _ [] = []
    connectedHelper gg (n:ns) = let
      reachableNodes = reachable gg n
      unvisited = filter (`notMember` reachableNodes) ns
      in reachableNodes:connectedHelper gg unvisited
