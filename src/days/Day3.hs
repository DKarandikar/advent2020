module Day3 where

day3part1 :: IO ()
day3part1 = do
    input <- readFile "input/day3input.txt"
    print $ doDay3 3 1 input

doDay3 :: Int -> Int -> String -> Int
doDay3 right down s = 
    let 
        ls = lines s
        height = length ls
        width = length (head ls)
        divide = quotRem height down
        downwards = fst divide + (if snd divide /= 0 then 1 else 0)
    in 
         sum $ [if (ls!!(down*y)) !!((right*y) `mod` width) == '#' then 1 else 0 | y <- [1..downwards-1]]


directions :: [(Int, Int)]
directions = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

doPart2 :: String -> Int
doPart2 input = product $ map (\(r, d) -> doDay3 r d input) directions

day3part2 :: IO ()
day3part2 = do
    input <- readFile "input/day3input.txt"
    print $ doPart2 input
