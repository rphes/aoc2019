{-# LANGUAGE RecordWildCards #-}

import           Control.Monad.State
import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as IntSet
import           Data.List
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Debug.Trace

type Point = (Int, Int, Int)

type Bugs = Set Point

data Simulation =
  Simulation
    { iter   :: Int
    , points :: [Point]
    , bugs   :: Bugs
    , seen   :: IntSet
    , dim    :: Int
    }

countNeighbors bugs (x, y, z) =
  Set.size $
  Set.intersection
    bugs
    (Set.fromList [(x, y + 1, 0), (x + 1, y, 0), (x, y - 1, 0), (x - 1, y, 0)])

diversity width (x, y, _) = 2 ^ (width * y + x)

willBeBug :: Bugs -> Point -> Maybe Point
willBeBug bugs point =
  let isBug = point `Set.member` bugs
      count = countNeighbors bugs point
   in if (isBug && count == 1) || (not isBug && (count == 1 || count == 2))
        then Just point
        else Nothing

simulate :: Simulation -> Int
simulate sim@Simulation {..} =
  let bugs' = mapMaybe (willBeBug bugs) points
      diverse = sum $ map (diversity dim) bugs'
   in if diverse `IntSet.member` seen
        then diverse
        else simulate
               sim
                 { iter = iter + 1
                 , bugs = Set.fromList bugs'
                 , seen = IntSet.insert diverse seen
                 }

horizontalNeighbors y dim level = [(x, y, level + 1) | x <- [0 .. dim - 1]]

verticalNeighbors x dim level = [(x, y, level + 1) | y <- [0 .. dim - 1]]

recursiveNeighbors dim (x, y, level) =
  let center = dim `div` 2
      a
        | x == 0 = [(x + 1, y, level), (center - 1, center, level - 1)]
        | x == dim - 1 = [(center + 1, center, level - 1), (x - 1, y, level)]
        | x == center + 1 && y == center =
          (x + 1, y, level) : verticalNeighbors (dim - 1) dim level
        | x == center - 1 && y == center =
          (x - 1, y, level) : verticalNeighbors 0 dim level
        | otherwise = [(x + 1, y, level), (x - 1, y, level)]
      b
        | y == 0 = [(center, center - 1, level - 1), (x, y + 1, level)]
        | y == dim - 1 = [(x, y - 1, level), (center, center + 1, level - 1)]
        | y == center + 1 && x == center =
          (x, y + 1, level) : horizontalNeighbors (dim - 1) dim level
        | y == center - 1 && x == center =
          (x, y - 1, level) : horizontalNeighbors 0 dim level
        | otherwise = [(x, y + 1, level), (x, y - 1, level)]
   in a ++ b

willBeBug2 :: Bugs -> Int -> Point -> Maybe Point
willBeBug2 bugs dim point =
  let isBug = point `Set.member` bugs
      neighbors = recursiveNeighbors dim point
      count = Set.size (Set.intersection (Set.fromList neighbors) bugs)
   in if (isBug && count == 1) || (not isBug && (count == 1 || count == 2))
        then Just point
        else Nothing

simulate2 :: Simulation -> Int
simulate2 sim@Simulation {..} =
  let points' =
        points ++
        delete
          (2, 2, -iter - 1)
          (delete
             (2, 2, iter + 1)
             [ (x, y, z)
             | z <- [-iter - 1, iter + 1]
             , y <- [0 .. dim - 1]
             , x <- [0 .. dim - 1]
             ])
      bugs' = Set.fromList $ mapMaybe (willBeBug2 bugs dim) points'
   in if iter == 199
        then Set.size bugs'
        else simulate2 sim {iter = iter + 1, bugs = bugs', points = points'}

parse input =
  let dim = length input
      points = [(x, y, 0) | y <- [0 .. dim - 1], x <- [0 .. dim - 1]]
      bugs =
        Set.fromList $
        catMaybes $
        zipWith
          (\point char ->
             if char == '#'
               then Just point
               else Nothing)
          points
          (concat input)
   in (dim, points, bugs)

main = do
  contents <- readFile "input.txt"
  let lns = lines contents
      (dim, points, bugs) = parse lns
      initial =
        Simulation
          { iter = 0
          , points = points
          , bugs = bugs
          , seen = IntSet.empty
          , dim = dim
          }
  print $ simulate initial
  print $ simulate2 initial {points = delete (2, 2, 0) points}
