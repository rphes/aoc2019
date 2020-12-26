import           Data.List.Split
import           IntCode
import           Prelude

--                                Crew Quarters
--                                 (escape pod)
--                                      |
--                                 Engineering -------------------------------- Hull breach --- Sick Bay
--                                      |                                            |        (molten lava)
--                                   Storage                                         |
--                                                                                   |
--                   Science Lab -- Kitchen -------------------------- Hot Chocolate Spring Fountain
--                                 (hologram)                                   (astrolabe)
--                                      |                                            |
--  Corridor ----- Holodeck -------- Passages ----------- Warp Drive maintenance     |
-- (hypercube)     (wreath)  (space law space brochure)        (photons)             |
--                                                                                   |
--                                                               Navigation          |
--                                                                 (coin)            |
--                                                                   |               |
--                                                                 Arcade ------- Stables ------- Hallway
--                                                                                (cake)      (infinite loop)
--                                                                                                   |
--                                                                                              Observatory ------- Gift Wrapping Center
--                                                                                         (giant electromagnet)       (food ration)
--                                                                                                                           |
--                                                                                                                  Security Checkpoint (hypercube, cake, coin, hologram)
--                                                                                                                           |
--                                                                                                                           |
--                                                                                                                Pressure-sensitive Floor
ascii :: Program -> IO ()
ascii p@Program {status = Running} = maybe (return ()) ascii (step p)
ascii p@Program {status = status, outputs = o} =
  let output = map toEnum o
   in case status of
        Waiting -> do
          putStr $ reverse output
          command <- getLine
          ascii
            p
              { status = Running
              , inputs = map fromEnum $ command ++ "\n"
              , outputs = []
              }
        Stop -> do
          putStr output
          return ()

main = do
  contents <- readFile "input.txt"
  let prog = initProgram (map read $ splitOn "," contents :: [Int]) []
  ascii prog
