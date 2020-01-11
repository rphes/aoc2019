{-# LANGUAGE ViewPatterns #-}

import           Control.Monad
import           Data.List
import           Data.Semigroup
import           Data.Tuple
import           Debug.Trace

type Position = Int

dealNew :: Int -> Position -> Position
dealNew l x = l - 1 - x

cut :: Int -> Int -> Position -> Position
cut n l x = (x - n) `mod` l

dealWithInc :: Int -> Int -> Position -> Position
dealWithInc n l x = (x * n) `mod` l

step :: String -> Int -> Int -> Int
step "deal into new stack" numCards pos = dealNew numCards pos
step (stripPrefix "cut " -> Just n) numCards pos = cut (read n) numCards pos
step (stripPrefix "deal with increment " -> Just n) numCards pos =
  dealWithInc (read n) numCards pos

shuffle :: [String] -> Int -> Int -> Int
shuffle steps numCards pos = foldl (\p s -> step s numCards p) pos steps

part1 :: [String] -> Int
part1 steps = shuffle steps 10007 2019

main = do
  contents <- readFile "input.txt"
  print $ part1 (lines contents)
