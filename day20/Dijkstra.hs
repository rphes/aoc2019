{-# LANGUAGE ScopedTypeVariables #-}

module Dijkstra where

import           Data.Heap       (MinPrioHeap)
import qualified Data.Heap       as Heap
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Debug.Trace

type Graph a = Map a [(a, Int)]

dijkstra' ::
     (Eq a, Ord a)
  => a
  -> Graph a
  -> (MinPrioHeap Int a, Map a Int, Map a a)
  -> Maybe Int
dijkstra' target graph (queue, dist, prev) = do
  ((curr, node), queue') <- Heap.view queue
  if node == target
    then Just $ dist ! node
    else dijkstra' target graph $
         foldr
           (\(neighbor, len) (queue', dist', prev') ->
              let alt = (dist ! node) + len
               in if alt < Map.findWithDefault (maxBound :: Int) neighbor dist
                    then ( Heap.insert (alt, neighbor) queue'
                         , Map.insert neighbor alt dist'
                         , Map.insert neighbor node prev')
                    else (queue', dist', prev'))
           (queue', dist, prev)
           (graph ! node)

dijkstra ::
     forall a. (Eq a, Ord a)
  => a
  -> a
  -> Graph a
  -> Maybe Int
dijkstra source target graph =
  let dist = Map.singleton source 0
      prev = Map.empty
      queue = Heap.singleton (0, source) :: MinPrioHeap Int a
   in dijkstra' target graph (queue, dist, prev)
