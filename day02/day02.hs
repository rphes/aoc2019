import System.IO
import Data.List.Split
import Debug.Trace

update :: Int -> a -> [a] -> [a]
update at val xs = let (begin, _:end) = splitAt at xs
                    in begin ++ (val : end)

exec :: Int -> Int -> Int -> Int
exec 1 a b = a + b
exec 2 a b = a * b

step :: [Int] -> Int -> [Int]
step prog counter | counter < length prog =
  let (op:rest) = drop counter prog
   in case op of
        99 -> prog
        otherwise ->
          let (a':b':dest:_) = rest
              a = prog!!a'
              b = prog!!b'
           in step (update dest (exec op a b) prog) (counter + 4)
step prog counter | otherwise = prog

run :: [Int] -> Int -> Int -> Int
run prog noun verb = head $ step ( update 1 noun $ update 2 verb prog ) 0

part1 prog = run prog 12 2

gridsearch :: [Int] -> [(Int, Int)] -> (Int, Int)
gridsearch prog ((noun, verb):space) = let res = run prog noun verb
                                        in case res of
                                             19690720 -> (noun, verb)
                                             otherwise -> gridsearch prog space

part2 prog = let range = [0..99]
                 space = [(x, y) | x <- range, y <- range]
                 (noun, verb) = gridsearch prog space
              in 100 * noun + verb

main = do
  contents <- readFile "input.txt"
  let prog = map read (splitOn "," contents)::[Int]
  print $ "Part 1: " ++ (show $ part1 prog)
  print $ "Part 2: " ++ (show $ part2 prog)
