import           Data.Function   (on)
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace
import           IntCode         hiding (run)

type Node = (Int, Int)

type Path = [Node]

neighbors :: Node -> [(Int, Node)]
neighbors (x, y) =
  [(1, (x, y + 1)), (2, (x, y - 1)), (3, (x - 1, y)), (4, (x + 1, y))]

explore :: Program -> Node -> Set Node -> Path -> Maybe Path
explore prog@Program {outputs = [status]} node visited path =
  case status of
    1 ->
      explore
        prog {outputs = [], status = Running}
        node
        (Set.insert node visited)
        (node : path)
    2 -> Just (node : path)
    _ -> Nothing
explore prog@Program {status = Waiting} node visited path = do
  let ns = filter (\(_, n) -> Set.notMember n visited) (neighbors node)
  let paths =
        mapMaybe
          (\(input, node') ->
             explore
               prog {inputs = [input], status = Running}
               node'
               visited
               path)
          ns
  case paths of
    [] -> Nothing
    _  -> Just $ minimumBy (compare `on` length) paths
explore prog node visited path =
  step prog >>= \prog' -> explore prog' node visited path

main = do
  contents <- readFile "input.txt"
  let prog = initProgram (map read $ splitOn "," contents :: [Int]) []
  print $ length <$> explore prog (0, 0) Set.empty []
