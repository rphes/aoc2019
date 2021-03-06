{-# LANGUAGE LambdaCase #-}

import           BFS
import           Data.Char
import           Data.Foldable
import           Data.Function   (on)
import           Data.Heap       (MinPrioHeap)
import qualified Data.Heap       as Heap
import           Data.List
import           Data.List.Split
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Sequence   (Seq ((:<|)), (|>))
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set

type Point = (Int, Int)

type Path = [(Point, Char)]

type Maze = Map Point Char

type Portals = Map Point Point

type Bounds = ((Int, Int), (Int, Int))

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]

getNeighbors :: Point -> Maze -> Set Point -> [(Point, Char)]
getNeighbors point maze visited =
  Map.assocs $
  Map.filter (/= '#') $
  Map.restrictKeys
    maze
    (Set.difference (Set.fromList $ adjacentPoints point) visited)

adjacencyList :: Maze -> Portals -> Point -> Maybe [Point]
adjacencyList maze portals point =
  foldrM
    (\point' list ->
       case Map.lookup point' maze of
         Just '.' -> Just $ point' : list
         Just c
           | isUpper c ->
             case Map.lookup point portals of
               Just portal -> Just $ portal : list
               Nothing     -> Just list
           | otherwise -> Just list
         _ -> Nothing)
    []
    (adjacentPoints point)

findPortals maze =
  mapMaybe
    (\(p, letter1) -> do
       let neighbors = getNeighbors p maze Set.empty
       (_, letter2) <- find (isUpper . snd) neighbors
       (point, _) <- find ((== '.') . snd) neighbors
       Just (sort [letter1, letter2], point))
    (Map.assocs $ Map.filter isUpper maze)

getMaze :: String -> Maybe Maze
getMaze input = do
  width <- elemIndex '\n' input
  Just $
    Map.fromList $
    zip [(x, y) | y <- [0 ..], x <- [0 .. width - 1]] (filter (/= '\n') input)

initialize input = do
  maze <- getMaze input
  let portals = findPortals maze
  a@(_, start) <- find ((== "AA") . fst) portals
  b@(_, end) <- find ((== "ZZ") . fst) portals
  let grouped =
        groupBy ((==) `on` fst) $
        sortBy (compare `on` fst) (delete a (delete b portals))
      mapping =
        Map.fromList $
        concat $
        mapMaybe
          (\case
             [(_, p1), (_, p2)] -> Just [(p1, p2), (p2, p1)]
             _ -> Nothing)
          grouped
  Just (maze, mapping, start, end)

drawPath path maze =
  let maze' = foldr (\p m -> Map.insert p '*' m) maze path
      ((width, height), _) = Map.findMax maze
   in unlines $
      chunksOf (width + 1) $
      mapMaybe (maze' !?) [(x, y) | y <- [0 .. height], x <- [0 .. width]]

recursiveAdjacencyList ::
     Maze -> Portals -> Bounds -> (Int, Point) -> Maybe [(Int, Point)]
recursiveAdjacencyList maze portals ((xl, xr), (yt, yb)) (level, point@(x, y)) =
  foldrM
    (\point' list ->
       case Map.lookup point' maze of
         Just '.' -> Just $ (level, point') : list
         Just c
           | isUpper c ->
             case Map.lookup point portals of
               Just portal ->
                 let isOuter = x <= xl || x >= xr || y <= yt || y >= yb
                  in case (isOuter, level) of
                       (True, 0) -> Just list
                       (True, _) -> Just $ (level - 1, portal) : list
                       (_, _)    -> Just $ (level + 1, portal) : list
               Nothing -> Just list
           | otherwise -> Just list
         _ -> Nothing)
    []
    (adjacentPoints point)

run :: String -> Maybe (IO ())
run input = do
  (maze, portals, start, end) <- initialize input
  path1 <-
    bfs
      start
      end
      (adjacencyList maze portals)
      (flip (|>))
      (\case
         (h :<| t) -> Just (h, t)
         _ -> Nothing)
      Seq.empty
  ((xMax, yMax), _) <- Map.lookupMax maze
  let bounds = ((2, xMax - 2), (2, yMax - 2))
  path2 <-
    bfs
      (0, start)
      (0, end)
      (recursiveAdjacencyList maze portals bounds)
      Heap.insert
      Heap.view
      (Heap.empty :: MinPrioHeap Int Point)
  Just $ print (length path1 - 1) >> print (length path2 - 1)

main = do
  contents <- readFile "input.txt"
  fromMaybe (error "No solution") (run contents)
  -- putStr $ drawPath path maze
