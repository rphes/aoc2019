import           Data.Char
import           Data.List.Split
import           Data.List
import qualified Data.Map.Lazy                 as Map
import           Data.Map.Lazy                  ( Map )

type Layer = [Int]
type PixelMap = Map (Int, Int) Int

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

constructLayers :: [Int] -> [Layer]
constructLayers = chunksOf (6 * 25)

layer2map :: Layer -> PixelMap
layer2map layer = Map.filter (/= 2)
  $ Map.fromList (zip [ (x, y) | x <- [0 .. 5], y <- [0 .. 24] ] layer)

toChar :: Int -> String
toChar 0 = "█"
toChar 1 = " "

printImage []     = return ()
printImage pixels = do
  let (row, rest) = splitAt 25 pixels
  putStrLn $ concatMap toChar row
  printImage rest

reduce layer curr@(currLayer, currCount) | c < currCount = (layer, c)
                                         | otherwise     = curr
  where c = count (== 0) layer

main = do
  contents <- map digitToInt <$> readFile "input.txt"
  let layers     = constructLayers contents
      (layer, _) = foldr reduce ([], 6 * 25) layers
  print $ count (== 1) layer * count (== 2) layer
  let image = Map.unions $ map layer2map layers
  printImage $ Map.elems image
