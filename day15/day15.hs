import           Control.Monad
import           Data.Function   (on)
import           Data.List       hiding ((\\))
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set        (Set, (\\))
import qualified Data.Set        as Set
import           Debug.Trace
import           IntCode         hiding (run)

type Node = (Int, Int)

type Path = [Node]

type NodeList = Map Node (Set Node)

neighbors :: Node -> [(Int, Node)]
neighbors (x, y) =
  [(1, (x, y + 1)), (2, (x, y - 1)), (3, (x - 1, y)), (4, (x + 1, y))]

explore ::
     Program
  -> Node
  -> Set Node
  -> Path
  -> NodeList
  -> Maybe (Maybe Path, NodeList)
explore prog@Program {outputs = [status]} node visited path nodemap =
  let previous = head path
      nodemap' =
        Map.unionWith
          Set.union
          (Map.fromList
             [(previous, Set.singleton node), (node, Set.singleton previous)])
          nodemap
      didVisit = Set.member node visited
   in case status of
        1 ->
          if didVisit
            then Just (Nothing, nodemap')
            else explore
                   prog {outputs = [], status = Running}
                   node
                   (Set.insert node visited)
                   (node : path)
                   nodemap'
        2 -> Just (Just (node : path), nodemap')
        _ -> Nothing
explore prog@Program {status = Waiting} node visited path nodemap = do
  let (paths, nodelists) =
        unzip $
        mapMaybe
          (\(input, node') ->
             explore
               prog {inputs = [input], status = Running}
               node'
               visited
               path
               nodemap)
          (neighbors node)
      paths' = catMaybes paths
      lengths =
        case paths' of
          [] -> Nothing
          _  -> Just $ minimumBy (compare `on` length) paths'
  Just (lengths, Map.unionsWith Set.union nodelists)
explore prog node visited path nodemap =
  step prog >>= \prog' -> explore prog' node visited path nodemap

fillOxygen :: NodeList -> Set Node -> Int -> [Node] -> Maybe Int
fillOxygen nodemap hasOxygen steps nodes = do
  ns <- Set.unions <$> traverse (`Map.lookup` nodemap) nodes
  let toFill = ns \\ hasOxygen
  if Set.size toFill > 0
    then fillOxygen
           nodemap
           (Set.union hasOxygen ns)
           (steps + 1)
           (Set.toList toFill)
    else Just steps

main = do
  contents <- readFile "input.txt"
  let prog = initProgram (map read $ splitOn "," contents :: [Int]) []
      res = explore prog (0, 0) Set.empty [(0, 0)] Map.empty
      shortestPath = subtract 1 . length <$> (fst =<< res)
      target = head $ fromJust $ fst $ fromJust res
  print shortestPath
  print target
  print $
    res >>= \(_, nodemap) ->
      fillOxygen nodemap (Set.singleton target) 0 [target]
