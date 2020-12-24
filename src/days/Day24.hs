module Day24 where

import qualified Data.Map as M

data Direction = E | SE | SW | W | NE | NW deriving (Eq, Show)

type Coord = (Int, Int, Int)
type HexGrid = M.Map Coord Bool

day24part1 :: IO ()
day24part1 = do
    input <- map (getLoc . parseLine) . lines <$> readFile "input/day24input.txt"

    print $ getCount $ foldr addCoord M.empty input


getCount :: HexGrid -> Int
getCount h = length $ filter ((==True) . snd) $ M.toList h

addCoord :: Coord -> HexGrid -> HexGrid
addCoord c h = case M.lookup c h of
    Just x -> M.insert c (not x) h
    Nothing -> M.insert c True h


parseLine :: String -> [Direction]
parseLine [] = []
parseLine (s : ss)
    | s == 'e' = E : parseLine ss
    | s == 'w' = W : parseLine ss
    | [s, head ss] == "se" = SE : parseLine (tail ss)
    | [s, head ss] == "sw" = SW : parseLine (tail ss)
    | [s, head ss] == "ne" = NE : parseLine (tail ss)
    | [s, head ss] == "nw" = NW : parseLine (tail ss)

getLoc :: [Direction] -> Coord
getLoc [] = (0, 0, 0)
getLoc (d:d')
    | d == E = (1, -1, 0) `add` getLoc d'
    | d == SE = (0, -1, 1) `add` getLoc d'
    | d == SW = (-1, 0, 1) `add` getLoc d'
    | d == W = (-1, 1, 0) `add` getLoc d'
    | d == NE = (1, 0, -1) `add` getLoc d'
    | d == NW = (0, 1, -1) `add` getLoc d'


add :: Coord -> Coord -> Coord
add (x, y, z) (x', y', z') = (x+x', y+y', z+z')


day24part2 :: IO ()
day24part2 = do
    input <- map (getLoc . parseLine) . lines <$> readFile "input/day24input.txt"

    let initialMap = foldr addCoord M.empty input

    print $ getCount $ iterate doStep initialMap !! 100


directions :: [Coord]
directions = [(1, -1, 0), (0, -1, 1), (-1, 0, 1), (-1, 1, 0), (1, 0, -1), (0, 1, -1)]

getInterestingLocations :: HexGrid -> [Coord]
getInterestingLocations m =  M.keys m ++ [add c dc | dc <- directions, c <- M.keys m]

getNeighbours :: Coord -> [Coord]
getNeighbours c = [add c dc | dc <- directions]

doStep :: HexGrid -> HexGrid
doStep h = 
    let
        interestingHexes = getInterestingLocations h
    in 
        M.fromList [stepFn pos h | pos <- interestingHexes]


stepFn :: Coord -> HexGrid -> (Coord, Bool)
stepFn c h =
    let 
        neighbours = getNeighbours c
        aliveNeighbours = length $ filter (==True) [M.findWithDefault False p h | p <- neighbours]
        myState = M.findWithDefault False c h
    in 
        if myState && (aliveNeighbours == 0 || aliveNeighbours > 2) then (c, False)
            else if not myState && (aliveNeighbours == 2) then (c, True)
                else (c, myState)
