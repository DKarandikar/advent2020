module Day11 where

import qualified Data.Map as M
import Data.Maybe ( fromMaybe )

import Utils ( runTilEqual )


data SeatLocState = Occupied | Empty | Floor | Invalid deriving Eq
type Coord = (Int, Int)
type SeatMap = M.Map Coord SeatLocState


day11part1 :: IO ()
day11part1 = do
    input <- readFile "input/day11input.txt"
    let seatMap = getSeatMap input
    let finalState = runTilEqual (applyStep getNextState) seatMap
    print $ sum $ [getSeatOccupied z finalState | z <- M.keys finalState]


day11part2 :: IO ()
day11part2 = do
    input <- readFile "input/day11input.txt"
    let seatMap = getSeatMap input
    let finalState = runTilEqual (applyStep getNextStateP2) seatMap
    print $ sum $ [getSeatOccupied z finalState | z <- M.keys finalState]


charToState:: Char -> SeatLocState
charToState c 
    | c == '.'  = Floor
    | c == 'L'  = Empty
    | c == '#'  = Occupied
    | c == 'B'  = Invalid


getSeatMap :: String -> SeatMap
getSeatMap input = M.fromList [
        ((x, y), charToState $ (ls!!y)!!x)
        | let ls = lines input, y <- [0 .. length ls -1 ], x <- [0.. length (ls!!y) - 1]
    ]


applyStep :: (Coord -> SeatMap -> (Coord, SeatLocState)) -> SeatMap -> SeatMap
applyStep stepFn map = M.fromList [stepFn pos map | pos <- M.keys map]

getSeatOccupied :: Coord -> SeatMap -> Int
getSeatOccupied z smap = 
    case M.lookup z smap of
        Just c -> if c == Occupied then 1 else 0
        Nothing -> 0 

getSeatState :: Coord -> SeatMap -> SeatLocState
getSeatState z smap = fromMaybe Invalid (M.lookup z smap)


dirs :: [Coord]
dirs = [(x', y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1], x' /= 0 || y'/= 0]

getNextState :: Coord -> SeatMap -> (Coord, SeatLocState)
getNextState (x, y) smap = 
    let 
        adjacentOccupied = sum [getSeatOccupied (x + x', y + y') smap | (x', y') <- dirs]
        currentState = getSeatState (x, y) smap
        res
            | currentState == Floor = Floor
            | (currentState == Empty && adjacentOccupied == 0) = Occupied
            | (currentState == Occupied && adjacentOccupied >= 4) = Empty
            | otherwise = currentState
    in 
        ((x, y), res)


getNextStateP2 :: Coord -> SeatMap -> (Coord, SeatLocState)
getNextStateP2 (x, y) smap = 
    let 
        adjacentOccupied = sum [getSeatOccupied z smap | z <- getVisible (x, y) smap]
        currentState = getSeatState (x, y) smap
        res
            | currentState == Floor = Floor
            | (currentState == Empty && adjacentOccupied == 0) = Occupied
            | (currentState == Occupied && adjacentOccupied >= 5) = Empty
            | otherwise = currentState
    in 
        ((x, y), res)

getVisible :: Coord -> SeatMap -> [Coord]
getVisible (x, y) smap =  
        let 
            getFirstNonFloor = head . dropWhile ((== Floor) . (`getSeatState` smap))
            vs = [ getFirstNonFloor [(x + n*x', y + n*y') | n <- [1..]] | (x', y') <- dirs]
        in 
            filter (\z-> getSeatState z smap /= Invalid) vs   
