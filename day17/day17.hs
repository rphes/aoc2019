{-# LANGUAGE LambdaCase #-}

import           Data.Char
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe
import           Data.Tuple
import           Debug.Trace
import           IntCode
import           Prelude            hiding (Left, Right)

data Direction
  = Up
  | Right
  | Down
  | Left
  deriving (Enum, Eq, Show)

data Turn
  = R
  | L
  | N
  deriving (Eq, Show)

type Image = Map (Int, Int) Char

type Path = [(Turn, Int)]

ascii :: Program -> Maybe String
ascii prog = do
  Program {outputs = outputs} <- run prog
  Just $ map toEnum outputs

buildMap :: String -> Maybe Image
buildMap img = do
  width <- elemIndex '\n' img
  Just $
    Map.fromList $
    zip [(x, y) | y <- [0 ..], x <- [0 .. width - 1]] (filter (/= '\n') img)

directions = [Up, Right, Down, Left]

opposite d =
  case d of
    Up    -> Down
    Right -> Left
    Down  -> Up
    Left  -> Right

neighbors (x, y) = [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]

neighborByDirection d (x, y) =
  case d of
    Up    -> (x, y - 1)
    Right -> (x + 1, y)
    Down  -> (x, y + 1)
    Left  -> (x - 1, y)

directionsToTurn from to =
  case (from, to) of
    (Up, Right)   -> 'R'
    (Up, Left)    -> 'L'
    (Right, Down) -> 'R'
    (Right, Up)   -> 'L'
    (Down, Left)  -> 'R'
    (Down, Right) -> 'L'
    (Left, Up)    -> 'R'
    (Left, Down)  -> 'L'
    _             -> 'N'

isIntersection key imap =
  all (\k -> Map.findWithDefault ' ' k imap == '#') (neighbors key)

intersects imap =
  let scaffolds = Map.filter (== '#') imap
   in Map.filterWithKey (\k _ -> isIntersection k imap) scaffolds

part1 prog = do
  img <- ascii prog
  imap <- buildMap img
  let ix = intersects imap
      params = map (uncurry (*)) (Map.keys ix)
  Just (img, sum params)

getDirections :: (Int, Int) -> Image -> Direction -> [Direction] -> [Direction]
getDirections key scaffolds direction path =
  let dirs =
        filter
          (\d ->
             d /= opposite direction &&
             Map.member (neighborByDirection d key) scaffolds)
          directions
   in case dirs of
        [] -> reverse path
        d ->
          let nextDir =
                if direction `elem` d
                  then direction
                  else head dirs
           in getDirections
                (neighborByDirection nextDir key)
                scaffolds
                nextDir
                (nextDir : path)

directionsToPath dirs = zipWith directionsToTurn dirs (tail dirs)

match :: String -> [String] -> [String] -> Int -> Maybe ([String], [String])
match xs segments path count
  | count > 10 = Nothing
  | length segments > 3 = Nothing
  | null xs = Just (reverse segments, reverse path)
  | otherwise =
    let prefixes = filter (`isPrefixOf` xs) segments
        res1 =
          mapMaybe
            (\s -> match (drop (length s) xs) segments (s : path) (count + 1))
            prefixes
        res2 =
          mapMaybe
            (\i ->
               let (s, xs') = splitAt i xs
                in match xs' (s : segments) (s : path) count)
            [1 .. length $ concat $ take 10 (group xs)]
     in if not (null res1 && null res2)
          then Just $ head (res1 ++ res2)
          else Nothing

encode :: [String] -> [String] -> Maybe (String, [String])
encode segments path = do
  let smap = Map.fromList $ zip segments ['A' ..]
      functions =
        map
          (intercalate "," .
           map
             (\case
                "L" -> "L"
                "R" -> "R"
                g -> show $ length g + 1) .
           group)
          segments
  mainRoutine <- intersperse ',' <$> traverse (smap Map.!?) path
  Just (mainRoutine, functions)

toInputs mainRoutine functions =
  map fromEnum $ mainRoutine ++ "\n" ++ intercalate "\n" functions ++ "\nn\n"

part2 prog@Program {register = register} = do
  img <- ascii prog
  imap <- buildMap img
  let scaffolds = Map.filter (== '#') imap
      initial = head $ Map.keys $ Map.filter (== '^') imap
      dirs = getDirections initial scaffolds Up [Up]
  (segments, path) <- match (directionsToPath dirs) [] [] 0
  (mainRoutine, functions) <- encode segments path
  let inputs = toInputs mainRoutine functions
      register' = IntMap.insert 0 2 register
  run prog {register = register', inputs = inputs}

main = do
  contents <- readFile "input.txt"
  let prog = initProgram (map read $ splitOn "," contents :: [Int]) []
      p1 = fromJust $ part1 prog
      Program {outputs = p2} = fromJust $ part2 prog
  print $ snd p1
  -- putStr $ fst p1
  print p2
