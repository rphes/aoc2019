import           Control.Exception
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace
import           System.IO

data Direction
  = North
  | East
  | South
  | West
  deriving (Show)

data Path =
  Path Direction Int
  deriving (Show)

data Point =
  Point Int Int
  deriving (Show)

data Line
  = HLine Int Int Int
  | VLine Int Int Int
  deriving (Show)

traceThis a = trace (show a) a

parsePath :: String -> Maybe Path
parsePath (d:len) = do
  dir <-
    case d of
      'U' -> Just North
      'R' -> Just East
      'D' -> Just South
      'L' -> Just West
      _   -> Nothing
  Just (Path dir (read len :: Int))

parseWire :: [String] -> Maybe [Path]
parseWire = mapM parsePath

path2line :: Point -> Path -> (Line, Point, Int)
path2line (Point x y) path =
  case path of
    (Path North l) -> (VLine x y (y + l), Point x (y + l), l)
    (Path East l)  -> (HLine x (x + l) y, Point (x + l) y, l)
    (Path South l) -> (VLine x y (y - l), Point x (y - l), l)
    (Path West l)  -> (HLine x (x - l) y, Point (x - l) y, l)

getLines :: Maybe [Path] -> [(Line, Int)]
getLines (Just paths) = fst $ foldl reduce ([], (Point 0 0, 0)) paths
  where
    reduce (lst, (point, len)) path =
      ((line, len) : lst, (newPoint, len + newLen))
      where
        (line, newPoint, newLen) = path2line point path
getLines _ = []

isBetween :: Int -> (Int, Int) -> Bool
isBetween x (a, b) = (a <= x) && (x <= b) || (b <= x) && (x <= a)

intersect :: (Line, Int) -> (Line, Int) -> Maybe (Point, Int)
intersect (HLine x1 x2 y, l1) (VLine x y1 y2, l2)
  | x == 0 && y == 0 = Nothing
  | isBetween x (x1, x2) && isBetween y (y1, y2) =
    Just (Point x y, l1 + l2 + abs (x1 - x) + abs (y1 - y))
  | otherwise = Nothing
intersect (VLine x y1 y2, l1) (HLine x1 x2 y, l2) =
  (HLine x1 x2 y, l2) `intersect` (VLine x y1 y2, l1)
intersect _ _ = Nothing

intersectAll :: [(Line, Int)] -> [(Line, Int)] -> [(Point, Int)]
intersectAll wire1 wire2 =
  mapMaybe (uncurry intersect) [(l1, l2) | l1 <- wire1, l2 <- wire2]

dist :: Point -> Int
dist (Point x y) = abs x + abs y

getIntersections text = intersectAll wire1 wire2
  where
    (paths1:paths2:_) = map (splitOn ",") (lines text)
    wire1 = getLines $ parseWire paths1
    wire2 = getLines $ parseWire paths2

part1 intersections = minimum $ map (\(point, _) -> dist point) intersections

part2 intersections = minimum $ map snd intersections

verify expected actual
  | expected == actual = return ()
  | otherwise =
    print $
    "Value did not match, expected " ++ show expected ++ ", got " ++ show actual

main = do
  let test0 = "R8,U5,L5,D3\nU7,R6,D4,L4"
  let test1 =
        "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
  let test2 =
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  contents <- readFile "input.txt"
  let ix0 = getIntersections test0
      ix1 = getIntersections test1
      ix2 = getIntersections test2
      ix_final = getIntersections contents
  verify 159 (part1 ix1)
  verify 6 (part1 ix0)
  verify 135 (part1 ix2)
  print $ "Part 1: " ++ show (part1 ix_final)
  verify 30 (part2 ix0)
  verify 610 (part2 ix1)
  verify 410 (part2 ix2)
  print $ "Part 2: " ++ show (part2 ix_final)
