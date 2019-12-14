import System.IO
import Data.List.Split
import Data.Maybe
import Control.Exception


data Direction = North | East | South | West deriving (Show)
data Path = Path Direction Int deriving (Show)
data Point = Point Int Int deriving (Show)
data Line = HLine Int Int Int | VLine Int Int Int deriving (Show)


parsePath :: String -> Maybe Path
parsePath (d:len) = do
  dir <- case d of
          'U' -> Just North
          'R' -> Just East
          'D' -> Just South
          'L' -> Just West
          _ -> Nothing
  Just (Path dir (read len::Int))


parseWire :: [String] -> Maybe [Path]
parseWire d = sequence $ map parsePath d


path2line :: Point -> Path -> (Line, Point, Int)
path2line (Point x y) path = case path of
                               (Path North l) -> ((VLine x y (y + l)), (Point x (y + l)), l)
                               (Path East l) -> ((HLine x (x + l) y), (Point (x + l) y), l)
                               (Path South l) -> ((VLine x y (y - l)), (Point x (y - l)), l)
                               (Path West l) -> ((HLine x (x - l) y), (Point (x -l) y), l)


getLines :: Maybe [Path] -> [Line]
getLines (Just paths) = fst $ foldl reduce ([], (Point 0 0)) paths where
  reduce (lst, point) path = 
    (line : lst, newPoint) where
    (line, newPoint, _) = path2line point path
getLines _ = []


isBetween :: Int -> (Int, Int) -> Bool
isBetween x (a, b) = (a <= x) && (x <= b) || (b <= x) && (x <= a)


intersect :: Line -> Line -> Maybe Point
intersect (HLine x1 x2 y) (VLine x y1 y2)
  | x == 0 && y == 0 = Nothing
  | isBetween x (x1, x2) && isBetween y (y1, y2) = Just (Point x y)
  | otherwise = Nothing
intersect (VLine x y1 y2) (HLine x1 x2 y) = intersect (HLine x1 x2 y) (VLine x y1 y2)
intersect _ _ = Nothing


intersectAll :: [Line] -> [Line] -> [Point]
intersectAll wire1 wire2 = catMaybes $ map (uncurry intersect) [(l1, l2) | l1 <- wire1, l2 <- wire2]


dist :: Point -> Int
dist (Point x y) = abs x + abs y


part1 text = minimum $ map dist crossings where 
  (paths1:paths2:_) = map (splitOn ",") (lines text)
  wire1 = getLines $ parseWire paths1
  wire2 = getLines $ parseWire paths2
  crossings = intersectAll wire1 wire2


main = do
  let test1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
  let test2 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  contents <- readFile "input.txt"

  assert ((part1 test1) == 159) return ()
  assert ((part1 test2) == 135) return ()
  print $ "Part 1: " ++ (show $ part1 contents)
