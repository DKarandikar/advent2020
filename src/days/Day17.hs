module Day17 where

import Data.List ( nub )
import qualified Data.Map as M

type Coord4 = (Int, Int, Int, Int)
type ConwayMap = M.Map Coord4 Bool
type CoordFilter = (Coord4 -> Bool)


day17part1 :: IO ()
day17part1 = do
    input <- readFile "input/day17input.txt"
    print $ runConwayCube input (\(x, y, z, w) -> w == 0) 5


day17part2 :: IO ()
day17part2 = do
    input <- readFile "input/day17input.txt"
    print $ runConwayCube input (const True) 5


runConwayCube :: String -> CoordFilter -> Int -> Int
runConwayCube s filterf iterations = length $ filter snd (M.toList m1)
    where m1 = foldr (\x m -> doStep filterf m) (getStartingMap s) [0..iterations]


getStartingMap :: String -> ConwayMap
getStartingMap input = M.fromList [
        ((x, y, 0, 0), charToState $ (ls!!y)!!x)
        | let ls = lines input, y <- [0 .. length ls -1 ], x <- [0.. length (ls!!y) - 1]
    ]
    where charToState = (/= '.')

add :: Coord4 -> Coord4 -> Coord4
add (x, y, z, w) (x', y', z', w') = (x + x', y + y', z + z', w + w')

directions :: [Coord4]
directions = [(x', y', z', w') | x' <- [-1 .. 1], y' <- [-1 .. 1], z' <- [-1 .. 1], w' <- [-1 .. 1], z' /= 0 || y' /= 0 || x' /= 0 || w' /= 0]

getInterestingLocations :: ConwayMap -> [Coord4]
getInterestingLocations m =  M.keys m ++ [add c dc | dc <- directions, c <- M.keys m]

getNeighbours :: Coord4 -> [Coord4]
getNeighbours c = [add c dc | dc <- directions]

doStep :: CoordFilter -> ConwayMap -> ConwayMap
doStep filterf m = 
    let 
        locs = filter filterf (getInterestingLocations m)
    in
        M.fromList [stepFn filterf pos m | pos <- locs]

stepFn :: CoordFilter -> Coord4 -> ConwayMap -> (Coord4, Bool)
stepFn filterf pos m = 
    let 
        neighbours = filter filterf (getNeighbours pos)
        aliveNeighbours = length $ filter (==True) [M.findWithDefault False p m | p <- neighbours]
        myState = M.findWithDefault False pos m
    in 
        (pos, (myState && (aliveNeighbours == 3 || aliveNeighbours == 2)) || (not myState && aliveNeighbours == 3))
