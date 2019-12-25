import           Data.List
import           Debug.Trace

data Moon =
  Moon
    { pos :: (Int, Int, Int)
    , vel :: (Int, Int, Int)
    }
  deriving (Eq, Show)

attract :: Moon -> Moon -> Moon
attract m@Moon {pos = (x1, y1, z1), vel = (vx, vy, vz)} Moon {pos = (x2, y2, z2)} =
  let f p1 p2 v
        | p1 > p2 = v - 1
        | p1 < p2 = v + 1
        | p1 == p2 = v
   in m {vel = (f x1 x2 vx, f y1 y2 vy, f z1 z2 vz)}

gravity :: [Moon] -> [Moon]
gravity moons =
  map (\moon -> foldr (flip attract) moon $ filter (/= moon) moons) moons

velocity :: [Moon] -> [Moon]
velocity =
  map
    (\m@Moon {vel = (vx, vy, vz), pos = (x, y, z)} ->
       m {pos = (x + vx, y + vy, z + vz)})

potentialEnergy :: Moon -> Int
potentialEnergy Moon {pos = (x, y, z)} = sum [abs x, abs y, abs z]

kineticEnergy :: Moon -> Int
kineticEnergy Moon {vel = (vx, vy, vz)} = sum [abs vx, abs vy, abs vz]

energy :: Moon -> Int
energy moon = potentialEnergy moon * kineticEnergy moon

initMoons :: [(Int, Int, Int)] -> [Moon]
initMoons = map (\p -> Moon {pos = p, vel = (0, 0, 0)})

step :: [Moon] -> [Moon]
step = velocity . gravity

simulate :: Int -> [Moon] -> [Moon]
simulate 0 moons     = moons
simulate steps moons = simulate (steps - 1) $ step moons

findPeriods steps moons getter values
  | steps > 0 && getter moons == values = steps
  | otherwise = findPeriods (steps + 1) (step moons) getter values

getX = map (\Moon {pos = (x, _, _), vel = (vx, _, _)} -> (x, vx))

getY = map (\Moon {pos = (_, y, _), vel = (_, vy, _)} -> (y, vy))

getZ = map (\Moon {pos = (_, _, z), vel = (_, _, vz)} -> (z, vz))

findSteps moons =
  let (p:ps) =
        traceShowId
          [ findPeriods 0 moons getX (getX moons)
          , findPeriods 0 moons getY (getY moons)
          , findPeriods 0 moons getZ (getZ moons)
          ]
   in foldr lcm p ps

main = do
  let testMoons1 = initMoons [(-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1)]
      testMoons2 = initMoons [(-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3)]
      moons =
        initMoons [(-4, -9, -3), (-13, -11, 0), (-17, -7, 15), (-16, 4, 2)]
      moons' = simulate 1000 moons
  print $ sum (map energy moons')
  print $ findSteps moons
