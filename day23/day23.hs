import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans
import           Data.Char
import           Data.Foldable
import           Data.IntMap.Strict     (IntMap, (!?))
import qualified Data.IntMap.Strict     as IntMap
import           Data.IntSet            (IntSet)
import qualified Data.IntSet            as IntSet
import           Data.List.Split
import           Debug.Trace
import           IntCode                hiding (run)

data StopSignal =
  StopSignal
  deriving (Show, Eq)

type Packet = Either StopSignal (Int, Int)

type Network = IntMap (TQueue Packet)

type NAT = TVar (IntMap Int, Packet, Packet)

stopAll network nat origin =
  mapM_
    (\addr -> sendPacket network nat origin addr (Left StopSignal))
    (255 : IntMap.keys network)

threshold = 100

readPacket :: Network -> NAT -> Int -> STM (Maybe Packet)
readPacket network nat address =
  case network !? address of
    Just queue -> do
      maybePacket <- tryReadTQueue queue
      (idleMap, lastPacket, lastSent) <- readTVar nat
      case maybePacket of
        Just packet -> do
          writeTVar nat (IntMap.insert address 0 idleMap, lastPacket, lastSent)
          return $ Just packet
        Nothing -> do
          empty <- mapM isEmptyTQueue network
          when
            (IntMap.findWithDefault 0 address idleMap <= threshold)
            (writeTVar
               nat
               ( IntMap.insertWith (\_ v -> v + 1) address 1 idleMap
               , lastPacket
               , lastSent))
          when
            (IntMap.size (IntMap.filter (> threshold) idleMap) ==
             IntMap.size network &&
             and empty)
            (if lastPacket == lastSent
               then stopAll network nat address
               else do
                 writeTVar nat (IntMap.empty, lastPacket, lastPacket)
                 traceShow lastPacket $
                   sendPacket network nat address 0 lastPacket)
          return Nothing
    Nothing -> stopAll network nat address >> return Nothing

sendPacket :: Network -> NAT -> Int -> Int -> Packet -> STM ()
sendPacket network nat origin target packet = do
  (idleMap, oldPacket, lastSent) <- readTVar nat
  let idleMap' = IntMap.insert origin 0 idleMap
  if target == 255
    then trace
           ("Sending " ++ show packet ++ " to NAT")
           writeTVar
           nat
           (idleMap', packet, lastSent)
    else case network !? target of
           Just queue -> do
             writeTQueue queue packet
             writeTVar nat (idleMap', oldPacket, lastSent)
           Nothing -> trace "Sending stop signals" $ stopAll network nat origin

run :: Network -> NAT -> Int -> Program -> IO ()
run _ _ _ p@Program {status = Stop} = return ()
run network nat address p@Program {status = Waiting} = do
  read <- atomically $ readPacket network nat address
  case read of
    Just value ->
      case value of
        Left _ -> trace "Stopping" return ()
        Right (x, y) ->
          run network nat address p {inputs = [x, y], status = Running}
    Nothing -> run network nat address p {inputs = [-1], status = Running}
run network nat address p@Program {outputs = [y, x, address']} = do
  atomically $ sendPacket network nat address address' (Right (x, y))
  run network nat address p {outputs = []}
run network nat address p = mapM_ (run network nat address) (step p)

main = do
  contents <- readFile "input.txt"
  let addresses = [0 .. 49]
      prog = initProgram (map read $ splitOn "," contents :: [Int])
      progs = map (prog . (: [])) addresses
  block <- newEmptyMVar :: IO (MVar Int)
  queues <- replicateM 50 newTQueueIO
  nat <- newTVarIO (IntMap.empty, Right (0, 0), Right (0, 0))
  let network = IntMap.fromList $ zip addresses queues
  let workers = zipWith (run network nat) addresses progs
  mapM_ forkIO workers
  let go = do
        (_, packet, packet') <- readTVarIO nat
        if packet == Left StopSignal
          then print packet'
          else go
  go
