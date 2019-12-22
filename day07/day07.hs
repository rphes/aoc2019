import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.IntMap.Strict  (IntMap, (!))
import qualified Data.IntMap.Strict  as IntMap
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace

type Address = Int

type Register = IntMap Int

type Operator = Int -> Int -> Int

type Comparator = Int -> Int -> Bool

data Status
  = Running
  | Waiting
  | Stop
  deriving (Eq, Show)

data Program =
  Program
    { register :: Register
    , counter  :: Address
    , inputs   :: [Int]
    , outputs  :: [Int]
    , status   :: Status
    }
  deriving (Eq, Show)

data Mode
  = Position
  | Immediate
  deriving (Eq, Show)

data Op
  = Alu Operator Mode Mode
  | Jump Comparator Mode Mode
  | Input
  | Output Mode
  | Terminate

toMode :: Int -> Maybe Mode
toMode 0 = Just Position
toMode 1 = Just Immediate
toMode _ = Nothing

(##) :: [a] -> Int -> Maybe a
(x:_) ## 0 = Just x
[] ## _ = Nothing
(x:xs) ## n = xs ## (n - 1)

getMode :: [Mode] -> Int -> Mode
getMode modes idx =
  let mode = modes ## idx
   in fromMaybe Position mode

comp :: Comparator -> Operator
comp op a b
  | a `op` b = 1
  | otherwise = 0

lt = comp (<)

eq = comp (==)

decode :: Int -> Maybe Op
decode inst = do
  let (op', modes') = splitAt 2 $ reverse $ show inst
  let op = read $ reverse op'
  modes <- traverse (toMode . digitToInt) modes'
  let m1 = return $ getMode modes 0
      m2 = return $ getMode modes 1
  case op of
    1  -> liftA2 (Alu (+)) m1 m2
    2  -> liftA2 (Alu (*)) m1 m2
    3  -> Just Input
    4  -> fmap Output m1
    5  -> liftA2 (Jump (/=)) m1 m2
    6  -> liftA2 (Jump (==)) m1 m2
    7  -> liftA2 (Alu lt) m1 m2
    8  -> liftA2 (Alu eq) m1 m2
    99 -> Just Terminate
    _  -> Nothing

getReg :: Program -> Address -> Maybe Int
getReg Program {register = r} = flip IntMap.lookup r

load :: Program -> Mode -> Address -> Maybe Int
load p@Program {counter = c} Position  = (getReg p <=< getReg p) . (c +)
load p@Program {counter = c} Immediate = getReg p . (c +)

alu :: Program -> Operator -> Mode -> Mode -> Maybe Program
alu p@Program {register = r, counter = c} op m1 m2 = do
  a <- load p m1 1
  b <- load p m2 2
  addr <- load p Immediate 3
  let r' = IntMap.insert addr (a `op` b) r
  return p {register = r', counter = c + 4}

jump :: Program -> Comparator -> Mode -> Mode -> Maybe Program
jump p@Program {counter = c} comp m1 m2 = do
  val <- load p m1 1
  addr <- load p m2 2
  let c' =
        if val `comp` 0
          then addr
          else c + 3
  return p {counter = c'}

input :: Program -> Maybe Program
input p@Program {register = r, counter = c, inputs = (x:xs)} = do
  addr <- load p Immediate 1
  let r' = IntMap.insert addr x r
  return p {register = r', counter = c + 2, inputs = xs}
input p@Program {inputs = []} = return p {status = Waiting}

-- input p@Program {inputs = []} = return p {status = Waiting}
output :: Program -> Mode -> Maybe Program
output p@Program {counter = c, outputs = xs} m1 = do
  x <- load p m1 1
  let xs' = x : xs
  return p {counter = c + 2, outputs = xs'}

step :: Program -> Maybe Program
step p@Program {register = r, counter = c} = do
  op <- load p Immediate 0 >>= decode
  case op of
    Alu op m1 m2    -> alu p op m1 m2
    Input           -> input p
    Jump comp m1 m2 -> jump p comp m1 m2
    Output mode     -> output p mode
    Terminate       -> Just p {status = Stop}

initRegister :: [Int] -> Register
initRegister = IntMap.fromList . zip [0 ..]

initProgram :: [Int] -> [Int] -> Program
initProgram memory inputs =
  Program
    { register = initRegister memory
    , inputs = inputs
    , outputs = []
    , counter = 0
    , status = Running
    }

run :: Program -> Maybe Program
run p@Program {status = Stop}    = Just p
run p@Program {status = Waiting} = Just p
run p                            = step p >>= run

runMultiple :: [Program] -> Maybe Program
runMultiple (p@Program {status = Waiting}:Program {status = Waiting}:_) =
  Nothing
runMultiple ps@(p@Program {status = Stop}:Program {status = Stop}:_) = Just p
runMultiple (p@Program {status = status, outputs = outputs}:p2@Program {inputs = inputs}:ps)
  | status == Waiting =
    runMultiple
      (p2 {inputs = inputs ++ outputs} :
       ps ++ [p {status = Running, outputs = []}])
  | status == Stop =
    runMultiple (p2 {inputs = inputs ++ outputs} : ps ++ [p {outputs = []}])
  | otherwise = step p >>= \p' -> runMultiple (p' : p2 : ps)

verifyProg :: Monad m => Program -> [Int] -> m ()
verifyProg prog expected =
  assert ((register <$> run prog) == Just (initRegister expected)) return ()

verifyOutput :: Monad m => Program -> Int -> m ()
verifyOutput prog expected =
  assert ((outputs <$> run prog) == Just [expected]) return ()

search :: ([Int] -> Program) -> [Int] -> Maybe Int
search p args = do
  let space = map (\(x:xs) -> (x ++ [0]) : xs) $ permutations $ map (: []) args
      applied = map (map p) space
  ps <- traverse runMultiple applied
  let res = map (\Program {outputs = outputs} -> head outputs) ps
  Just $ maximum res

main = do
  contents <- readFile "input.txt"
  let init = initProgram (map read (splitOn "," contents) :: [Int])
  print $ search init [0, 1, 2, 3, 4]
  print $ search init [5, 6, 7, 8, 9]
