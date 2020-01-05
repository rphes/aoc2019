module BFS where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence   (Seq ((:<|)), (|>))
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set

bfs' ::
     (Eq a, Ord a, Show a)
  => a
  -> (a -> Maybe [a])
  -> Seq a
  -> Set a
  -> Map a a
  -> Maybe (Map a a)
bfs' target lookupF Seq.Empty visited prev = Nothing
bfs' target lookupF (node :<| queue) visited prev =
  if node == target
    then Just prev
    else do
      neighbors <- lookupF node
      let neighbors' = filter (`Set.notMember` visited) neighbors
          prev' =
            foldr (\neigh prev' -> Map.insert neigh node prev') prev neighbors'
          queue' = foldr (flip (|>)) queue neighbors'
          visited' = foldr Set.insert visited neighbors'
      bfs' target lookupF queue' visited' prev'

backtrack :: (Eq a, Ord a, Show a) => [a] -> Map a a -> Maybe [a]
backtrack path@(x:_) prev =
  case Map.lookup x prev of
    Just n  -> backtrack (n : path) prev
    Nothing -> Just $ reverse path

bfs :: (Eq a, Ord a, Show a) => a -> a -> (a -> Maybe [a]) -> Maybe [a]
bfs source target lookupF =
  let queue = Seq.singleton source
      visited = Set.singleton source
   in bfs' target lookupF queue visited Map.empty >>= backtrack [target]
