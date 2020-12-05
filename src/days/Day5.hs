module Day5 where

import Utils ( removeItem )

day5part1 :: IO ()
day5part1 = do
    input <- readFile "input/day5input.txt"
    print $ maximum $ map getId (lines input)


calcId :: (Int, Int) -> Int
calcId (r, c) = (r * 8) + c

getId:: String -> Int
getId s = (getRow s * 8) +  getColumn s

getRow :: String -> Int
getRow s = getVal 128 'F' (take 7 s)

getColumn :: String -> Int
getColumn s = getVal 8 'L' (drop 7 s)

getVal:: Int -> Char -> String -> Int
getVal start c s = fst $ foldl (\(l, h) x -> if x == c then (l, l + ((h-l) `div` 2)) else (l + ((h-l) `div` 2), h)) (0, start) s

day5part2 :: IO ()
day5part2 = do
    input <- readFile "input/day5input.txt"
    let seats = [(x, y) | x <- [0..127], y <- [0..7]]
    let remSeats = foldl (\a (x,y) -> removeItem (x, y) a ) seats [(getRow s, getColumn s) | s <- lines input]
    print $ findSeat remSeats

findSeat :: [(Int, Int)] -> Int
findSeat = calcId . (!!1) . findSeat'

-- Remove any seats where the next id is the same, this should leave 2 seats, the final 'front' seat, and our seat
findSeat':: [(Int, Int)] -> [(Int, Int)]
findSeat' [] = []
findSeat' [_] = []
findSeat' (a : s) = if calcId a + 1 /= calcId (head s) then a : findSeat' s else findSeat' s