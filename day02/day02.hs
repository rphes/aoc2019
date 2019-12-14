import System.IO
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Lazy as M
import qualified Data.IntMap.Lazy as IntMap
import Control.Exception
import Debug.Trace

import Control.Monad ((>=>))
import Data.List.Split

step :: IntMap Int -> Int -> Maybe (IntMap Int)
step prog counter = do
  let getReg = flip M.lookup prog
  let loadWord = getReg >=> getReg
  inst <- getReg counter
  case inst of
    99 -> Just prog
    _ -> do
      a <- loadWord (counter + 1)
      b <- loadWord (counter + 2)
      dest <- getReg (counter + 3)
      let exec op = step (IntMap.insert dest (a `op` b) (prog)) (counter + 4)
      case inst of
        1 -> exec (+)
        2 -> exec (*)
        _ -> Nothing

run :: IntMap Int -> Int -> Int -> Maybe Int
run prog noun verb = do
  prog <- step (IntMap.insert 1 noun $ IntMap.insert 2 verb prog) 0
  M.lookup 0 prog

part1 prog = case run prog 12 2 of
               Just res -> res
               Nothing -> error "No result"

gridsearch :: IntMap Int -> Int -> [(Int, Int)] -> (Int, Int)
gridsearch prog value ((noun, verb):space)
  | res == Just value = (noun, verb)
  | otherwise = gridsearch prog value space
  where res = run prog noun verb

constructRegister :: [Int] -> IntMap Int
constructRegister = IntMap.fromList . zip [0..]

part2 prog = let range = [0..99]
                 space = [(x, y) | x <- range, y <- range]
                 (noun, verb) = gridsearch prog 19690720 space
              in 100 * noun + verb


traceThis a = trace (show a) a

verify input output =
  assert (step (constructRegister input) 0 == Just (constructRegister output)) return ()

main = do
  contents <- readFile "input.txt"
  let prog = constructRegister $ (map read (splitOn "," contents) :: [Int])

  verify [1,9,10,3,2,3,11,0,99,30,40,50] [3500,9,10,70,2,3,11,0,99,30,40,50]
  verify [1,0,0,0,99] [2,0,0,0,99]
  verify [2,3,0,3,99] [2,3,0,6,99]
  verify [2,4,4,5,99,0] [2,4,4,5,99,9801]
  verify [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99]

  print $ "Part 1: " ++ (show $ part1 prog)
  print $ "Part 2: " ++ (show $ part2 prog)
