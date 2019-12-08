import System.IO

fuel :: Int -> Int

fuel mass | mass > 0 =
  let f = max ((div mass 3) - 2) 0
  in f + (fuel f)

fuel mass | otherwise = 0

main = do
  contents <- readFile "input.txt"
  let mass = map read (lines contents)
  print $ sum (map fuel mass)
