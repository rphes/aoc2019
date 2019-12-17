import           Control.Exception
import           Control.Monad      ((>=>))
import           Data.Char
import           Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace

decode :: Int -> (Int, [Int])
decode inst = (read opcode, reverse $ map digitToInt paramModes)
  where
    s = show inst
    (paramModes, opcode) = splitAt (length s - 2) s

load :: IntMap Int -> Int -> Int -> Maybe Int
load prog mode val =
  case mode of
    0 -> IntMap.lookup val prog
    1 -> Just val
    _ -> Nothing

mode :: Int -> [Int] -> Int
mode idx modes
  | idx < length modes = modes !! idx
  | otherwise = 0

eval :: IntMap Int -> Int -> [Int] -> [Int] -> Maybe ([Int], IntMap Int)
eval prog counter input output = do
  let getReg = flip IntMap.lookup prog
  (opcode, modes) <- fmap decode (getReg counter)
  let getMode = flip mode modes
  let dest = getReg (counter + 3)
  case opcode of
    3
      -- load input
     -> do
      let (x:xs) = input
      dest <- getReg (counter + 1)
      eval (IntMap.insert dest x prog) (counter + 2) xs output
    4
      -- store output
     -> do
      p <- getReg (counter + 1)
      val <- load prog (getMode 0) p
      eval prog (counter + 2) input (val : output)
    5
      -- jump-if-true
     -> do
      a <- (getReg >=> load prog (getMode 0)) (counter + 1)
      b <- (getReg >=> load prog (getMode 1)) (counter + 2)
      eval
        prog
        (if a /= 0
           then b
           else counter + 3)
        input
        output
    6
      -- jump-if-false
     -> do
      a <- (getReg >=> load prog (getMode 0)) (counter + 1)
      b <- (getReg >=> load prog (getMode 1)) (counter + 2)
      eval
        prog
        (if a == 0
           then b
           else counter + 3)
        input
        output
    7
      -- less than
     -> do
      a <- (getReg >=> load prog (getMode 0)) (counter + 1)
      b <- (getReg >=> load prog (getMode 1)) (counter + 2)
      dest <- getReg (counter + 3)
      eval
        (IntMap.insert
           dest
           (if a < b
              then 1
              else 0)
           prog)
        (counter + 4)
        input
        output
    8
      -- equals
     -> do
      a <- (getReg >=> load prog (getMode 0)) (counter + 1)
      b <- (getReg >=> load prog (getMode 1)) (counter + 2)
      dest <- getReg (counter + 3)
      eval
        (IntMap.insert
           dest
           (if a == b
              then 1
              else 0)
           prog)
        (counter + 4)
        input
        output
    99 -> Just (reverse output, prog)
    _ -> do
      a <- (getReg >=> load prog (getMode 0)) (counter + 1)
      b <- (getReg >=> load prog (getMode 1)) (counter + 2)
      dest <- getReg (counter + 3)
      let exec op =
            eval (IntMap.insert dest (a `op` b) prog) (counter + 4) input output
      case opcode of
        1 -> exec (+)
        2 -> exec (*)
        _ -> Nothing

initProg :: [Int] -> IntMap Int
initProg = IntMap.fromList . zip [0 ..]

run prog input = eval (initProg prog) 0 (reverse input) []

verifyProg prog input expected =
  assert (snd (fromJust $ run prog input) == initProg expected) return ()

verifyOutput prog input expected =
  assert (fst (fromJust $ run prog input) == expected) return ()

main = do
  let test1 = [3, 0, 4, 0, 99]
      test2 = [1002, 4, 3, 4, 33]
      test3 = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
      test4 = [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]
      test5 = [3, 3, 1108, -1, 8, 3, 4, 3, 99]
      test6 = [3, 3, 1107, -1, 8, 3, 4, 3, 99]
  verifyOutput test1 [1] [1]
  verifyProg test2 [1] [1002, 4, 3, 4, 99]
  verifyOutput test3 [8] [1]
  verifyOutput test4 [8] [0]
  verifyOutput test5 [7] [0]
  verifyOutput test6 [1] [1]
  contents <- readFile "input.txt"
  let prog = map read (splitOn "," contents) :: [Int]
  print $ fmap fst (run prog [1])
  print $ fmap fst (run prog [5])
