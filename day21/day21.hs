import           Data.Char
import           Data.List.Split
import           Debug.Trace
import           IntCode

formatOutput prog =
  let output =
        case prog of
          Just Program {outputs = o} -> o
          Nothing                    -> []
      (text, damage) = span (< 256) output
      damage' =
        case damage of
          (d:_) -> show d
          _     -> ""
   in unlines [map chr text, damage']

-- Robot jumps 4 steps
-- Cases:
-- #####.###########
-- #####...#########
-- #####..#.########
part1 prog =
  let input = map fromEnum "NOT A J\nAND D J\nNOT C T\nAND D T\nOR T J\nWALK\n"
   in formatOutput $ run (prog input)

main = do
  contents <- readFile "input.txt"
  let prog = initProgram (map read $ splitOn "," contents :: [Int])
  putStr $ part1 prog
