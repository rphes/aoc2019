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

comp op a b
  | a `op` b = 1
  | otherwise = 0

lt = comp (<)

eq = comp (==)

alu prog counter input output a b addr op = do
  a <- a
  b <- b
  addr <- addr
  eval (IntMap.insert addr (a `op` b) prog) (counter + 4) input output

jump prog counter input output val addr comp = do
  val <- val
  addr <- addr
  let new_counter
        | val `comp` 0 = addr
        | otherwise = counter + 3
   in eval prog new_counter input output

eval :: IntMap Int -> Int -> [Int] -> [Int] -> Maybe ([Int], IntMap Int)
eval prog counter input output = do
  let getReg = flip IntMap.lookup prog . (counter +)
  (opcode, modes) <- decode <$> getReg 0
  let getParam p = (getReg >=> load prog (mode (p - 1) modes)) p
  let p1 = getParam 1
      p2 = getParam 2
  let alu' = alu prog counter input output
  let jump' = jump prog counter input output
  case opcode of
    1 -> alu' p1 p2 (getReg 3) (+)
    2 -> alu' p1 p2 (getReg 3) (*)
    3
      -- load input
     -> do
      let (x:xs) = input
      dest <- getReg 1
      eval (IntMap.insert dest x prog) (counter + 2) xs output
    4
      -- store output
     -> do
      val <- p1
      eval prog (counter + 2) input (val : output)
    5 -> jump' p1 p2 (/=) -- jump-if-true
    6 -> jump' p1 p2 (==) -- jump-if-false
    7 -> alu' p1 p2 (getReg 3) lt -- less than
    8 -> alu' p1 p2 (getReg 3) eq -- equals
    99 -> Just (reverse output, prog)
    _ -> Nothing

initProg :: [Int] -> IntMap Int
initProg = IntMap.fromList . zip [0 ..]

run prog input = eval (initProg prog) 0 (reverse input) []

verifyProg prog input expected =
  assert (snd (fromJust $ run prog input) == initProg expected) return ()

verifyOutput prog input expected =
  assert (fst (fromJust $ run prog input) == expected) return ()

main = do
  contents <- readFile "input2.txt"
  let prog = map read (splitOn "," contents) :: [Int]
  print $ fmap fst (run prog [1])
  print $ fmap fst (run prog [5])
