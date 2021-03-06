module Day12 where

data CardinalDir = North | East | South | West deriving (Show, Eq, Enum, Bounded)

type ShipState = (CardinalDir, (Int, Int))
type ShipWaypointState = ((Int, Int), (Int, Int))

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
    where
      add mod x y = (x + y + mod) `rem` mod


day12part1 :: IO ()
day12part1 = do
    input <- readFile "input/day12input.txt"
    let res = foldl handleLine (East, (0,0)) (lines input)
    print res
    print $ abs (fst $ snd res) + abs (snd $ snd res)
    

day12part2 :: IO ()
day12part2 = do
    input <- readFile "input/day12input.txt"
    let res = foldl handleLine2 ((10,1), (0,0)) (lines input)
    print res
    print $ abs (fst $ snd res) + abs (snd $ snd res)


handleLine:: ShipState -> String -> ShipState
handleLine (orientation, (locX, locY)) instruction = 
    let 
        action = head instruction
        val = read (drop 1 instruction) :: Int
    in 
        case action of
            'F' -> case orientation of
                North -> (orientation, (locX, locY + val))
                East -> (orientation, (locX + val, locY))
                West -> (orientation, (locX - val, locY))
                South -> (orientation, (locX, locY - val))
            'N' -> (orientation, (locX, locY + val))
            'E' -> (orientation, (locX + val, locY))
            'S' -> (orientation, (locX, locY - val))
            'W' -> (orientation, (locX - val, locY))
            'R' -> (getClockwiseTurn orientation val, (locX, locY))
            'L' -> (getAntiClockwiseTurn orientation val, (locX, locY))

getClockwiseTurn :: CardinalDir -> Int -> CardinalDir
getClockwiseTurn dir val = turn (val `div` 90) dir

getAntiClockwiseTurn :: CardinalDir -> Int -> CardinalDir
getAntiClockwiseTurn dir val = turn ( negate (val `div` 90)) dir


handleLine2:: ShipWaypointState -> String -> ShipWaypointState
handleLine2 ((locXW, locYW), (locX, locY)) instruction = 
    let 
        action = head instruction
        val = read (drop 1 instruction) :: Int
    in 
        case action of
            'F' -> ((locXW, locYW), (locX + (val * locXW), locY + (val*locYW)))
            'N' -> ((locXW, locYW + val), (locX, locY))
            'E' -> ((locXW + val, locYW), (locX, locY))
            'S' -> ((locXW, locYW - val), (locX, locY))
            'W' -> ((locXW - val, locYW), (locX, locY))
            'R' -> (getClockwiseTurnWayPoint (locXW, locYW) val, (locX, locY))
            'L' -> (getAntiClockwiseTurnWayPoint (locXW, locYW) val, (locX, locY))

getClockwiseTurnWayPoint :: (Int, Int) -> Int -> (Int, Int)
getClockwiseTurnWayPoint (x, y) val = 
    case val of 
        90 -> (y, -x)
        180 -> (-x, -y)
        270 -> (-y, x)
        360 -> (x, y)

getAntiClockwiseTurnWayPoint :: (Int, Int) -> Int -> (Int, Int)
getAntiClockwiseTurnWayPoint (x, y) val = 
    case val of 
        270 -> (y, -x)
        180 -> (-x, -y)
        90 -> (-y, x)
        360 -> (x, y)
