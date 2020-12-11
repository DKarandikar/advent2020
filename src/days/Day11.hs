module Day11 where

import qualified Data.Map as M

type SeatMap = M.Map (Int, Int) Char

day11part1 :: IO ()
day11part1 = do
    input <- readFile "input/day11input.txt"
    let seatMap = getSeatMap input
    let finalState = simplify seatMap
    print $ sum $ [getSeatOccupied z finalState | z <- M.keys finalState]


handleLine:: String -> Int -> SeatMap
handleLine line lineNo = M.fromList [((x, lineNo), line!!x) | x <- [0.. (length line) - 1]]

getSeatMap :: String -> SeatMap
getSeatMap input = M.fromList [((x, lineNo), ((lines input)!!lineNo)!!x) | lineNo <- [0 .. length (lines input) -1], x <- [0.. (length ((lines input)!!lineNo)) - 1]]

applyStep :: SeatMap -> SeatMap
applyStep map = M.fromList [getNextState pos map | pos <- M.keys map]

getNextState :: (Int, Int) -> SeatMap -> ((Int, Int), Char)
getNextState (x, y) smap = 
    let 
        adjacentOccupied = sum [getSeatOccupied z smap | z <- [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x-1, y-1), (x-1, y+1), (x+1, y-1)]]
        currentState = getSeatState (x, y) smap
        res = if currentState == '.' then '.' else 
            if (currentState == 'L' && adjacentOccupied == 0) then '#' else
                if (currentState == '#' && adjacentOccupied >= 4) then 'L' else currentState
    in 
        ((x, y), res)

getSeatOccupied :: (Int, Int) -> SeatMap -> Int
getSeatOccupied z smap = 
    case M.lookup z smap of
        Just c -> if c == '#' then 1 else 0
        Nothing -> 0 

getSeatState :: (Int, Int) -> SeatMap -> Char
getSeatState z smap = 
    case M.lookup z smap of
        Just c -> c
        Nothing -> 'B'


converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

simplify = converge (==) . iterate applyStep

simplifyP2 = converge (==) . iterate applyStepP2

applyStepP2 :: SeatMap -> SeatMap
applyStepP2 map = M.fromList [getNextStateP2 pos map | pos <- M.keys map]

getNextStateP2 :: (Int, Int) -> SeatMap -> ((Int, Int), Char)
getNextStateP2 (x, y) smap = 
    let 
        adjacentOccupied = sum [getSeatOccupied z smap | z <- getVisible (x, y) smap]
        currentState = getSeatState (x, y) smap
        res = if currentState == '.' then '.' else 
            if (currentState == 'L' && adjacentOccupied == 0) then '#' else
                if (currentState == '#' && adjacentOccupied >= 5) then 'L' else currentState
    in 
        ((x, y), res)

getVisible :: (Int, Int) -> SeatMap -> [(Int, Int)]
getVisible (x, y) smap =  
        let 
            vs = [ head $ dropWhile ((\x -> x == '.') . (\z -> getSeatState z smap)) [(x + n*x', y + n*y') | n <- [1..]] | x' <- [-1, 0, 1], y' <- [-1, 0, 1], x' /= 0 || y'/= 0]
        in 
            filter (\z-> (getSeatState z smap) /= 'B') vs


day11part2 :: IO ()
day11part2 = do
    input <- readFile "input/day11input.txt"
    let seatMap = getSeatMap input
    let finalState = simplifyP2 seatMap
    print $ sum $ [getSeatOccupied z finalState | z <- M.keys finalState]
   
