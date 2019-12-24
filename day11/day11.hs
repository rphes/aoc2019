import           Data.Function   (on)
import           Data.List
import           Data.List.Split
import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map
import           Debug.Trace
import           IntCode         hiding (run)
import           Prelude         hiding (Left, Right)

data Color
  = Black
  | White
  deriving (Enum, Show)

data Turn
  = CounterClockwise -- 0 = Left
  | Clockwise -- 1 = Right
  deriving (Enum, Show)

data Direction
  = Up
  | Right
  | Down
  | Left
  deriving (Enum, Show)

data Robot =
  Robot
    { position  :: Position
    , direction :: Direction
    }

type Position = (Int, Int)

type Painting = Map Position (Color, Int)

rotate :: Direction -> Turn -> Direction
rotate direction CounterClockwise = toEnum $ (fromEnum direction - 1) `mod` 4
rotate direction Clockwise        = toEnum $ (fromEnum direction + 1) `mod` 4

translate :: Position -> Direction -> Position
translate (x, y) Up    = (x, y + 1)
translate (x, y) Right = (x + 1, y)
translate (x, y) Down  = (x, y - 1)
translate (x, y) Left  = (x - 1, y)

setColor position color painting =
  let f (Just (_, n)) = Just (color, n + 1)
      f Nothing       = Just (color, 1)
   in Map.alter f position painting

paint :: [Int] -> Position -> Painting -> Painting
paint outputs position painting =
  let [color, _] = outputs
   in setColor position (toEnum color) painting

move :: [Int] -> Robot -> Robot
move outputs r@Robot {direction = dir, position = pos} =
  let [_, turn] = outputs
      direction' = rotate dir $ toEnum turn
      position' = translate pos direction'
   in r {position = position', direction = direction'}

run :: Program -> Robot -> Painting -> Maybe Painting
run prog@Program {status = status, outputs = outputs} robot@Robot {position = position} painting =
  case status of
    Running -> step prog >>= (\prog' -> run prog' robot painting)
    Waiting ->
      let painting' = paint (reverse outputs) position painting
          robot'@Robot {position = position'} = move (reverse outputs) robot
          (color', _) = Map.findWithDefault (Black, 0) position' painting
          prog' =
            prog {inputs = [fromEnum color'], outputs = [], status = Running}
       in run prog' robot' painting'
    Stop -> Just $ paint outputs position painting

colorMap Black = "█"
colorMap White = " "

printPainting :: Maybe Painting -> IO ()
printPainting (Just painting) = do
  let keys = Map.keys painting
      (x, y) = (map fst keys, map snd keys)
      (xmin, xmax, ymin, ymax) = (minimum x, maximum x, minimum y, maximum y)
      xs = [xmin .. xmax]
      ys = [ymax .. -ymin]
      lines =
        unlines $
        map
          (\y ->
             concatMap
               (\x ->
                  colorMap $
                  fst $ Map.findWithDefault (Black, 0) (x, -y) painting)
               xs)
          ys
  putStr lines

main = do
  contents <- readFile "input.txt"
  let p1 = initProgram (map read (splitOn "," contents) :: [Int]) [0]
      painting1 = run p1 Robot {position = (0, 0), direction = Up} Map.empty
  print $ fmap Map.size painting1
  let p2 = initProgram (map read (splitOn "," contents) :: [Int]) [1]
      painting2 = run p2 Robot {position = (0, 0), direction = Up} Map.empty
  printPainting painting2
