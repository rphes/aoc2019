import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Tree

parse :: [String] -> Map String String
parse xs = Map.fromList $ map (tup . splitOn ")") xs
  where
    tup (a:b:_) = (b, a)

findCOM :: Maybe String -> Map String String -> Maybe [String] -> Maybe [String]
findCOM key mapping path =
  case key of
    Just "COM" -> path
    Just key -> findCOM (Map.lookup key mapping) mapping (fmap (key :) path)
    Nothing -> Nothing

count :: Map String String -> Maybe Int
count mapping = do
  counts <-
    mapM
      (\s -> length <$> findCOM (Just s) mapping (Just []))
      (Map.keys mapping)
  Just $ sum counts

xor :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
xor a b = Set.union a b `Set.difference` Set.intersection a b

findPath :: String -> String -> Map String String -> Maybe Int
findPath a b mapping = do
  pathA <- Set.fromList <$> findCOM (Just a) mapping (Just [])
  pathB <- Set.fromList <$> findCOM (Just b) mapping (Just [])
  Just $ flip (-) 2 . Set.size $ xor pathA pathB

main = do
  content <- readFile "input.txt"
  let mapping = parse (lines content)
  print $ count mapping
  print $ findPath "YOU" "SAN" mapping
