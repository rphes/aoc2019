import           Data.Maybe
import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Function                  ( on )

data Point = Point {x :: Int, y :: Int} deriving ( Eq)

instance  Show Point where
  show (Point x y) = "(" ++ show x ++ " " ++ show y ++ ")"

isAsteroid :: Char -> Point -> Maybe Point
isAsteroid val coord | val == '#' = Just coord
                     | otherwise  = Nothing

angle :: Point -> Point -> Double
angle (Point x1 y1) (Point x2 y2) =
  pi - atan2 (fromIntegral (x2 - x1)) (fromIntegral (y2 - y1))

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) =
  sqrt (fromIntegral (x2 - x1) ^ 2 + fromIntegral (y2 - y1) ^ 2)


main = do
  contents <- readFile "input.txt"
  let
    ls        = lines contents
    width     = length $ head ls
    height    = length ls
    points    = [ Point x y | y <- [0 .. height - 1], x <- [0 .. width - 1] ]
    asteroids = catMaybes $ zipWith isAsteroid (concat ls) points
    visible   = map
      (\a ->
        (a, Set.fromList $ map (angle a) $ snd $ partition (a ==) asteroids)
      )
      asteroids
    (point, value) = maximumBy (\a b -> snd a `compare` snd b)
                               (map (\(p, v) -> (p, length v)) visible)
    angles = map (\p -> (p, (angle point p, dist point p))) $ snd $ partition
      (point ==)
      asteroids
    (point2@(Point x y), _) =
      concat
          (transpose $ groupBy ((==) `on` (fst . snd)) $ sortBy
            (compare `on` snd)
            angles
          )
        !! (200 - 1)
  print (point, value)
  print (point2, x * 100 + y)
