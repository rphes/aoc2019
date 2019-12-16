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
  case opcode of
    3 -> do
      let (x:xs) = input
      dest <- getReg (counter + 1)
      eval (IntMap.insert dest x prog) (counter + 2) xs output
    4 -> do
      p <- getReg (counter + 1)
      val <- load prog (getMode 0) p
      eval prog (counter + 2) input (val : output)
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

main = do
  let test1 = [3, 0, 4, 0, 99]
      test2 = [1002, 4, 3, 4, 33]
  assert (fst (fromJust (run test1 [1])) == [1]) return ()
  assert
    (snd (fromJust (run test2 [1])) == initProg [1002, 4, 3, 4, 99])
    return
    ()
  contents <- readFile "input.txt"
  let prog = map read (splitOn "," contents) :: [Int]
  print $ fmap fst (run prog [1])
