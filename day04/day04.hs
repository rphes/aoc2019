import           Control.Exception
import           Data.List
import           Data.Maybe

match :: Int -> Maybe Int
match n =
  if any (uncurry (==)) pairs && all (uncurry (<=)) pairs
    then Just n
    else Nothing
  where
    s = show n
    pairs = zip s (tail s)

match2 n =
  if all (uncurry (<=)) pairs && elem 2 (map length $ group s)
    then Just n
    else Nothing
  where
    s = show n
    pairs = zip s (tail s)

main = do
  assert (match 111111 == Just 111111) return ()
  assert (isNothing $ match 223450) return ()
  assert (isNothing $ match 665432) return ()
  assert (isNothing $ match 123789) return ()
  let range = [130254 .. 678275]
  print $ length $ mapMaybe match range
  assert (isNothing $ match2 111111) return ()
  assert (isNothing $ match2 123444) return ()
  assert (match2 111122 == Just 111122) return ()
  print $ length $ mapMaybe match2 range
