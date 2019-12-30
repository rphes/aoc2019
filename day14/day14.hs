import           Control.Monad
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Tuple
import           Debug.Trace

type Reactions = Map String (Int, [(String, Int)])

type Remainders = Map String Int

parseReaction :: String -> Reactions
parseReaction line =
  let [left, right] = splitOn " => " line
      f [amount, chem] = (read amount :: Int, chem)
      (amount, prod) = f $ words right
   in Map.singleton prod (amount, map (swap . f . words) (splitOn ", " left))

ceilDiv :: Integral a => a -> a -> a
ceilDiv a b
  | m > 0 = d + 1
  | m < 0 = d - 1
  | m == 0 = d
  where
    (d, m) = divMod a b

react :: Int -> String -> Reactions -> Remainders -> Maybe (Int, Remainders)
react 0 _ _ remainders = Just (0, remainders)
react amount "ORE" _ remainders = Just (amount, remainders)
react amount prod reactions remainders = do
  (resultingAmount, reactants) <- Map.lookup prod reactions
  let remainder = Map.findWithDefault 0 prod remainders
      multiple = ceilDiv (max (amount - remainder) 0) resultingAmount
      remainder' = max (remainder - amount + resultingAmount * multiple) 0
      remainders' = Map.insert prod remainder' remainders
  (ore, rm) <-
    foldM
      (\a (pr, am) -> foldFunc reactions a (pr, am * multiple))
      (0, remainders')
      reactants
  Just (ore, rm)

foldFunc ::
     Reactions -> (Int, Remainders) -> (String, Int) -> Maybe (Int, Remainders)
foldFunc reactions (ore, rems) (reactant, amount) = do
  (ore', rems') <- react amount reactant reactions rems
  Just (ore' + ore, rems')

binSearch :: Reactions -> Int -> Int -> Int -> Int -> Int
binSearch reactions minVal maxVal target prev
  | res == target || res == prev = value
  | res > target = binSearch reactions minVal value target res
  | res < target = binSearch reactions value maxVal target res
  where
    value = minVal + round ((fromIntegral maxVal - fromIntegral minVal) / 2)
    res = fst $ fromJust $ react value "FUEL" reactions Map.empty

main = do
  contents <- readFile "input.txt"
  let reactions = Map.unions $ map parseReaction (lines contents)
  print $ fst <$> react 1 "FUEL" reactions Map.empty
  print $ binSearch reactions 0 50000000 1000000000000 0
