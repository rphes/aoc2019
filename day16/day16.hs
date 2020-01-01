import           Data.Char

patt :: Int -> [a] -> [a]
patt n = tail . cycle . concatMap (replicate n)

phase :: [Int] -> [Int] -> [Int]
phase y x =
  map (\k -> abs (sum (zipWith (*) (patt k y) x)) `mod` 10) [1 .. length x]

fft :: Int -> [Int] -> String -> String
fft n y x =
  let x' = map digitToInt x
   in map intToDigit $ iterate (phase y) x' !! n

fastFft n x =
  let x' = map digitToInt x
   in map intToDigit $ iterate (scanr (\a b -> (a + b) `mod` 10) 0) x' !! n

main = do
  let fft' = fft 100 [0, 1, 0, -1]
  contents <- readFile "input.txt"
  print $ take 8 (fft' contents)
  let offset = read $ take 7 contents :: Int
      digits = drop offset $ concat $ replicate 10000 contents
  print $ take 8 (fastFft 100 digits)
