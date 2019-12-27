import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Debug.Trace
import           IntCode

data State =
  State
    { xPaddle :: Int
    , xBall   :: Int
    , score   :: Int
    }

part1 :: Program -> Maybe Int
part1 p = do
  Program {outputs = outputs} <- run p
  return $ countBlocks outputs 0
  where
    countBlocks (_:_:2:rest) count = countBlocks rest (count + 1)
    countBlocks (_:_:_:rest) count = countBlocks rest count
    countBlocks [] count           = count

part2 :: Program -> State -> Maybe Int
part2 Program {status = Stop} State {score = score} = Just score
part2 p@Program {status = Waiting} s@State {xPaddle = xp, xBall = xb} =
  part2 p {inputs = [joystick], status = Running} s
  where
    joystick = signum (xb - xp)
part2 p@Program {outputs = outputs@[_, _, _]} s =
  let s' =
        case outputs of
          [3, _, x]      -> s {xPaddle = x}
          [4, _, x]      -> s {xBall = x}
          [score, 0, -1] -> s {score = score}
          _              -> s
   in part2 p {outputs = []} s'
part2 p s = step p >>= flip part2 s

main = do
  contents <- readFile "input.txt"
  let p@Program {register = r} =
        initProgram (map read (splitOn "," contents) :: [Int]) []
  print $ part1 p
  print $
    part2
      p {register = IntMap.insert 0 2 r}
      State {xPaddle = 0, xBall = 0, score = 0}
