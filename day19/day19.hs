import           Data.List.Split
import           Debug.Trace
import           IntCode

detect prog (x, y) = do
  Program {outputs = [output]} <- run $ prog [x, y]
  Just output

detectAll prog grid = do
  outputs <- traverse (detect prog) grid
  Just $ filter (== 1) outputs

image prog =
  let grid = [(x, y) | y <- [0 .. 49], x <- [0 .. 49]]
      toPixel 0 = '.'
      toPixel 1 = '#'
   in case traverse (detect prog) grid of
        Just outputs -> unlines $ chunksOf 50 $ map toPixel outputs
        _            -> error "Could not generate image"

part1 prog =
  let grid = [(x, y) | y <- [0 .. 49], x <- [0 .. 49]]
   in detectAll prog grid >>= (Just . length)

expand :: ((Int, Int) -> Maybe Int) -> Int -> Int -> Int -> Maybe Int
expand detect' n left row = do
  let findLeft x =
        case detect' (x, row) of
          Just 1 -> Just x
          Just 0 -> findLeft $ x + 1
          _      -> Nothing
  left' <- traceShowId $ findLeft left
  opposite <- detect' (left' + (n - 1), row - (n - 1))
  if opposite == 1
    then Just $ left' * 10000 + (row - (n - 1))
    else expand detect' n left' (row + 1)

part2 prog = expand (detect prog) 100 0 100

main = do
  contents <- readFile "input.txt"
  let prog = initProgram (map read $ splitOn "," contents :: [Int])
  -- putStr $ image prog
  print $ part1 prog
  print $ part2 prog
