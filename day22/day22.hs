{-# LANGUAGE ViewPatterns #-}

import           Data.List hiding (repeat)
import           Prelude   hiding (repeat)

type Perm = (Integer, Integer)

deal :: Integer -> Perm
deal n = (-1, n - 1)

cut :: Integer -> Perm
cut k = (1, -k)

inc :: Integer -> Perm
inc k = (k, 0)

combine :: Integer -> Perm -> Perm -> Perm
combine n (a, b) (c, d) = ((a * c) `mod` n, (b * c + d) `mod` n)

combineAll n = foldl1 (combine n)

shuffle :: Integer -> Integer -> Perm -> Integer
shuffle x n (a, b) = (x * a + b) `mod` n

step :: Integer -> String -> Perm
step n "deal into new stack"                          = deal n
step n (stripPrefix "cut " -> Just k)                 = cut (read k)
step n (stripPrefix "deal with increment " -> Just k) = inc (read k)

part1 :: Integer -> Integer -> [String] -> Integer
part1 x n input = shuffle x n (combineAll n (map (step n) input))

expMod :: Integer -> Integer -> Integer -> Integer
expMod _ _ 0 = 1
expMod n x k
  | even k = expMod n ((x * x) `mod` n) (div k 2) `mod` n
  | odd k = x * expMod n ((x * x) `mod` n) (div (k - 1) 2) `mod` n

modInv n x = expMod n x (n - 2)

repeat :: Integer -> Integer -> Perm -> Perm
repeat n t (a, b) =
  let power = expMod n a t
   in (power, b * (1 - power) * modInv n (1 - a))

invert :: Integer -> Perm -> Perm
invert n (a, b) =
  let inv = modInv n a
   in (inv, (-b * inv) `mod` n)

part2 :: Integer -> Integer -> Integer -> [String] -> Integer
part2 x n t input =
  let perm = combineAll n (map (step n) input)
      perm' = repeat n t perm
   in shuffle 2020 n (invert n perm')

main = do
  contents <- readFile "input.txt"
  print $ part1 2019 10007 (lines contents)
  print $ part2 2020 119315717514047 101741582076661 (lines contents)
