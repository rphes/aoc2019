module IntCode where


import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.IntMap.Strict             ( IntMap
                                                , (!)
                                                )
import qualified Data.IntMap.Strict            as IntMap
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
    , base :: Int
    }
  deriving (Eq, Show)

data Mode
  = Position
  | Immediate
  | Relative
  deriving (Eq, Show)

data Op
  = Alu Operator Mode Mode Mode
  | Jump Comparator Mode Mode
  | Input Mode
  | Output Mode
  | SetBase Mode
  | Terminate

toMode :: Int -> Maybe Mode
toMode 0 = Just Position
toMode 1 = Just Immediate
toMode 2 = Just Relative
toMode _ = Nothing

(##) :: [a] -> Int -> Maybe a
(x : _)  ## 0 = Just x
[]       ## _ = Nothing
(x : xs) ## n = xs ## (n - 1)

getMode :: [Mode] -> Int -> Mode
getMode modes idx = let mode = modes ## idx in fromMaybe Position mode

comp :: Comparator -> Operator
comp op a b | a `op` b  = 1
            | otherwise = 0

lt = comp (<)

eq = comp (==)

decode :: Int -> Maybe Op
decode inst = do
  let (op', modes') = splitAt 2 $ reverse $ show inst
  let op            = read $ reverse op'
  modes <- traverse (toMode . digitToInt) modes'
  let m1 = return $ getMode modes 0
      m2 = return $ getMode modes 1
      m3 = return $ getMode modes 2
  case op of
    1  -> liftA3 (Alu (+)) m1 m2 m3
    2  -> liftA3 (Alu (*)) m1 m2 m3
    3  -> fmap Input m1
    4  -> fmap Output m1
    5  -> liftA2 (Jump (/=)) m1 m2
    6  -> liftA2 (Jump (==)) m1 m2
    7  -> liftA3 (Alu lt) m1 m2 m3
    8  -> liftA3 (Alu eq) m1 m2 m3
    9  -> fmap SetBase m1
    99 -> Just Terminate
    _  -> Nothing

getReg :: Program -> Address -> Int
getReg Program { register = r } = flip (IntMap.findWithDefault 0) r

load :: Program -> Mode -> Address -> Int
load p@Program { counter = c } Position  = getReg p . getReg p . (c +)
load p@Program { counter = c } Immediate = getReg p . (c +)
load p@Program { counter = c, base = b } Relative =
  getReg p . (b +) . getReg p . (c +)

store :: Program -> Mode -> Address -> Int -> Maybe Register
store p@Program { counter = c, register = r } Position addr val =
  Just $ IntMap.insert (getReg p (c + addr)) val r
store _ Immediate _ _ = Nothing
store p@Program { counter = c, base = b, register = r } Relative addr val =
  Just $ IntMap.insert (getReg p (c + addr) + b) val r

alu :: Program -> Operator -> Mode -> Mode -> Mode -> Maybe Program
alu p@Program { register = r, counter = c } op m1 m2 m3 = do
  let a = load p m1 1
      b = load p m2 2
  r' <- store p m3 3 (a `op` b)
  return p { register = r', counter = c + 4 }

jump :: Program -> Comparator -> Mode -> Mode -> Maybe Program
jump p@Program { counter = c } comp m1 m2 = do
  let val  = load p m1 1
      addr = load p m2 2
      c'   = if val `comp` 0 then addr else c + 3
  return p { counter = c' }

input :: Program -> Mode -> Maybe Program
input p@Program { register = r, counter = c, inputs = (x : xs) } mode = do
  r' <- store p mode 1 x
  return p { register = r', counter = c + 2, inputs = xs }
input p@Program { inputs = [] } _ = return p { status = Waiting }

output :: Program -> Mode -> Maybe Program
output p@Program { counter = c, outputs = xs } m1 = do
  let x   = load p m1 1
      xs' = x : xs
  return p { counter = c + 2, outputs = xs' }

setbase :: Program -> Mode -> Maybe Program
setbase p@Program { counter = c, base = b } m1 = do
  let inc = load p m1 1
  return p { counter = c + 2, base = b + inc }

step :: Program -> Maybe Program
step p@Program { register = r, counter = c, outputs = o } = do
  op <- decode $ load p Immediate 0
  case op of
    Alu op m1 m2 m3 -> alu p op m1 m2 m3
    Input mode      -> input p mode
    Jump comp m1 m2 -> jump p comp m1 m2
    Output  mode    -> output p mode
    SetBase mode    -> setbase p mode
    Terminate       -> Just p { status = Stop, outputs = reverse o }

initRegister :: [Int] -> Register
initRegister = IntMap.fromList . zip [0 ..]

initProgram :: [Int] -> [Int] -> Program
initProgram memory inputs = Program { register = initRegister memory
                                    , inputs   = inputs
                                    , outputs  = []
                                    , counter  = 0
                                    , status   = Running
                                    , base     = 0
                                    }

run :: Program -> Maybe Program
run p@Program { status = Stop }    = Just p
run p@Program { status = Waiting } = Just p
run p                              = step p >>= run
