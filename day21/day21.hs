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
  let input =
        map fromEnum $
        unlines ["NOT A J", "AND D J", "NOT C T", "AND D T", "OR T J", "WALK"]
   in formatOutput $ run (prog input)

-- Robot jumps 4 steps
-- Cases:
-- #####.###########
-- #####...#########
-- #####..#.########
-- #####.#.##..#.###
-- #####.##.########
-- #####.#..########
part2 prog =
  let input =
        map fromEnum $
        unlines
          [ "NOT A J"
          , "AND D J"
          , "NOT B T"
          , "AND D T"
          , "OR T J"
          , "NOT C T"
          , "AND D T"
          , "AND H T"
          , "OR T J"
          , "RUN"
          ]
   in formatOutput $ run (prog input)

main = do
  contents <- readFile "input.txt"
  let prog = initProgram (map read $ splitOn "," contents :: [Int])
  putStr $ part1 prog
  putStr $ part2 prog
