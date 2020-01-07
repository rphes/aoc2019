module BFS where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

bfs' ::
     (Eq a, Ord a, Show a)
  => a
  -> (a -> Maybe [a])
  -> (a -> b -> b)
  -> (b -> Maybe (a, b))
  -> b
  -> Set a
  -> Map a a
  -> Maybe (Map a a)
bfs' target lookupF enqueueF dequeueF queue visited prev = do
  (node, queue') <- dequeueF queue
  if node == target
    then Just prev
    else do
      neighbors <- lookupF node
      let neighbors' = filter (`Set.notMember` visited) neighbors
          prev' =
            foldr (\neigh prev' -> Map.insert neigh node prev') prev neighbors'
          queue'' = foldr enqueueF queue' neighbors'
          visited' = foldr Set.insert visited neighbors'
      bfs' target lookupF enqueueF dequeueF queue'' visited' prev'

backtrack :: (Eq a, Ord a, Show a) => [a] -> Map a a -> Maybe [a]
backtrack path@(x:_) prev =
  case Map.lookup x prev of
    Just n  -> backtrack (n : path) prev
    Nothing -> Just $ reverse path

bfs ::
     (Eq a, Ord a, Show a)
  => a
  -> a
  -> (a -> Maybe [a])
  -> (a -> b -> b)
  -> (b -> Maybe (a, b))
  -> b
  -> Maybe [a]
bfs source target lookupF enqueueF dequeueF emptyQueue =
  let queue = enqueueF source emptyQueue
      visited = Set.singleton source
   in bfs' target lookupF enqueueF dequeueF queue visited Map.empty >>=
      backtrack [target]
