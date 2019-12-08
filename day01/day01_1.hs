import System.IO

fuel :: Int -> Int
fuel mass = (div mass 3) - 2

main = do
  contents <- readFile "input.txt"
  let mass = map read (lines contents)
  print $ sum (map fuel mass)
