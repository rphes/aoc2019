import System.IO
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import Data.List.Split

step :: IntMap Int -> Int -> IntMap Int
step prog counter | counter < length prog =
  let inst = prog!counter
      a = prog!(prog!(counter + 1))
      b = prog!(prog!(counter + 2))
      dest = prog!(counter + 3)
      exec op = step (IntMap.insert dest (a `op` b) prog) (counter + 4) in
        case inst of
          1 -> exec (+)
          2 -> exec (*)
          99 -> prog
          _ -> error "Unsupported instruction"
step prog counter | otherwise = prog

run :: IntMap Int -> Int -> Int -> Int
run prog noun verb = (step (
  IntMap.insert 1 noun $
    IntMap.insert 2 verb
  prog) 0) ! 0

part1 prog = run prog 12 2

gridsearch :: IntMap Int -> Int -> [(Int, Int)] -> (Int, Int)
gridsearch prog value ((noun, verb):space)
  | res == value = (noun, verb)
  | res /= value  = gridsearch prog value space
  where res = run prog noun verb

part2 prog = let range = [0..99]
                 space = [(x, y) | x <- range, y <- range]
                 (noun, verb) = gridsearch prog 19690720 space
              in 100 * noun + verb

main = do
  contents <- readFile "input.txt"
  let prog = IntMap.fromList $ zip [0..] $ (map read (splitOn "," contents) :: [Int])

  print $ "Part 1: " ++ (show $ part1 prog)
  print $ "Part 2: " ++ (show $ part2 prog)
