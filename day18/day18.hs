-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE LambdaCase #-}
import           Data.Char
import           Data.Function   (on)
import           Data.List
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace

type Point = (Int, Int)

type Path = [(Point, Char)]

type Edge = (Char, Char, Int, Set Char)

type Partials = Map (Char, Set Char) Int

type Maze = Map Point Char

initialize :: String -> Maybe (Maze, Set Char, [(Point, Char)])
initialize input = do
  width <- elemIndex '\n' input
  let keysToFind = Set.fromList $ filter isLower input
      maze =
        Map.fromList $
        zip
          [(x, y) | y <- [0 ..], x <- [0 .. width - 1]]
          (filter (/= '\n') input)
      origins = Map.assocs $ Map.filter (\v -> isLower v || v == '@') maze
  Just (maze, keysToFind, origins)

getNeighbors :: Point -> Maze -> Set Point -> [(Point, Char)]
getNeighbors (x, y) maze visited =
  Map.assocs $
  Map.filter (/= '#') $
  Map.restrictKeys
    maze
    (Set.difference
       (Set.fromList [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)])
       visited)

phase1 :: [Path] -> [Path] -> Set Point -> Maze -> [Path]
phase1 [] found _ _ = found
phase1 paths found visited maze =
  let (visited', paths') =
        mapAccumL
          (\acc path@((k, v):_) ->
             let neighbors = getNeighbors k maze visited
              in ( Set.union acc $ Set.fromList (map fst neighbors)
                 , map (: path) neighbors))
          visited
          paths
      paths'' = concat paths'
      found' = filter (\((_, v):_) -> isLower v) paths''
   in phase1 paths'' (found' ++ found) visited' maze

process :: Char -> Path -> Edge
process origin path =
  let check v l k d
        | isLower v = (l + 1, v : k, d)
        | isUpper v =
          let v' = toLower v
           in if toLower v' `elem` k
                then (l + 1, k, d)
                else (l + 1, k, v' : d)
        | otherwise = (l + 1, k, d)
      (len, _, doors) =
        foldr
          (\(_, v) (len, keys, doors) -> check v len keys doors)
          (0, "", "")
          (reverse path)
   in (origin, snd $ head path, len - 1, Set.fromList doors)

phase2 :: Char -> [Edge] -> Set Char -> Partials -> Int -> Maybe (Partials, Int)
phase2 current edges keysToFind partials len
  | null keysToFind = Just (partials, len)
  | otherwise =
    case partials !? (current, keysToFind) of
      Just len' -> Just (partials, len' + len)
      Nothing ->
        let edges' =
              filter
                (\(o, dst, _, doors) ->
                   o == current &&
                   Set.disjoint keysToFind doors && dst `Set.member` keysToFind)
                edges
            (partials', results) =
              mapAccumR
                (\p (origin, dest, len', doors) ->
                   case phase2
                          dest
                          edges
                          (Set.delete dest keysToFind)
                          p
                          (len + len') of
                     Just (p', result) ->
                       ( Map.insertWith
                           min
                           (current, keysToFind)
                           (result - len)
                           p'
                       , Just result)
                     Nothing -> (p, Nothing))
                partials
                edges'
         in case catMaybes results of
              []      -> Nothing
              results -> Just (partials', minimum results)

part1 input = do
  (maze, keysToFind, origins) <- initialize input
  let edges =
        concatMap
          (\o@(k, v) -> map (process v) (phase1 [[o]] [] (Set.singleton k) maze))
          origins
  phase2 '@' edges keysToFind Map.empty 0

part2 inputs
  -- Hax!
 = do
  steps <- traverse part1 inputs
  Just $ sum (map snd steps)

main = do
  contents <- readFile "input.txt"
  let (_, steps) = fromJust $ part1 contents
  print steps
  contents2 <-
    mapM
      readFile
      ["input2_1.txt", "input2_2.txt", "input2_3.txt", "input2_4.txt"]
  let steps = fromJust $ part2 contents2
  print steps
