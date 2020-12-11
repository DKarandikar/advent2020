module Day11 where

import qualified Data.Map as M
import Data.Maybe ( fromMaybe )

import Utils ( runTilEqual )

type SeatMap = M.Map (Int, Int) Char

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


handleLine:: String -> Int -> SeatMap
handleLine line lineNo = M.fromList [((x, lineNo), line!!x) | x <- [0.. length line - 1]]

getSeatMap :: String -> SeatMap
getSeatMap input = M.fromList [((x, lineNo), (lines input!!lineNo)!!x) | lineNo <- [0 .. length (lines input) -1], x <- [0.. length (lines input!!lineNo) - 1]]


applyStep :: ((Int, Int) -> SeatMap -> ((Int, Int), Char)) -> SeatMap -> SeatMap
applyStep stepFn map = M.fromList [stepFn pos map | pos <- M.keys map]

getSeatOccupied :: (Int, Int) -> SeatMap -> Int
getSeatOccupied z smap = 
    case M.lookup z smap of
        Just c -> if c == '#' then 1 else 0
        Nothing -> 0 

getSeatState :: (Int, Int) -> SeatMap -> Char
getSeatState z smap = fromMaybe 'B' (M.lookup z smap)


dirs :: [(Int, Int)]
dirs = [(x', y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1], x' /= 0 || y'/= 0]

getNextState :: (Int, Int) -> SeatMap -> ((Int, Int), Char)
getNextState (x, y) smap = 
    let 
        adjacentOccupied = sum [getSeatOccupied (x + x', y + y') smap | (x', y') <- dirs]
        currentState = getSeatState (x, y) smap
        res
            | currentState == '.' = '.'
            | (currentState == 'L' && adjacentOccupied == 0) = '#'
            | (currentState == '#' && adjacentOccupied >= 4) = 'L'
            | otherwise = currentState
    in 
        ((x, y), res)


getNextStateP2 :: (Int, Int) -> SeatMap -> ((Int, Int), Char)
getNextStateP2 (x, y) smap = 
    let 
        adjacentOccupied = sum [getSeatOccupied z smap | z <- getVisible (x, y) smap]
        currentState = getSeatState (x, y) smap
        res
            | currentState == '.' = '.'
            | (currentState == 'L' && adjacentOccupied == 0) = '#'
            | (currentState == '#' && adjacentOccupied >= 5) = 'L'
            | otherwise = currentState
    in 
        ((x, y), res)

getVisible :: (Int, Int) -> SeatMap -> [(Int, Int)]
getVisible (x, y) smap =  
        let 
            vs = [ head $ dropWhile ((== '.') . (`getSeatState` smap)) [(x + n*x', y + n*y') | n <- [1..]] | (x', y') <- dirs]
        in 
            filter (\z-> getSeatState z smap /= 'B') vs   
