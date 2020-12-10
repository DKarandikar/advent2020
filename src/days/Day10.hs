module Day10 where

import Utils ( counts )
import Data.List ( sort )

day10part1 :: IO ()
day10part1 = do
    input <- readFile "input/day10input.txt"
    let ls = map (\x -> read x :: Int) (lines input) ++ [0]
    let sorted = sort ls
    let cs = counts $ [sorted!!x - sorted!!(x-1) | x <- [1..length ls - 1]]
    print $ snd (head cs) * (1 + snd (cs !! 1))


day10part2 :: IO ()
day10part2 = do
    input <- readFile "input/day10input.txt"
    let ls = map (\x -> read x :: Int) (lines input) ++ [0]
    print $ do10Part2 $ sort ls


do10Part2 :: [Int] -> Int
do10Part2 xs = snd $ last $ foldl f [] xs

f :: [(Int, Int)] -> Int -> [(Int, Int)]
f xs a = xs ++ [( a, max 1 (sum $ map snd (dropWhile ((<a-3) . fst) xs)) )]

-- e.g. 0 1 4 5 6 7 9 ...
-- ([(number, routes)] -> Int -> [[(number, routes)]) -> [[(number, routes)] -> [Int] -> [(number, routes)]
-- 0,1 - 1,1 - 4,1 - 5,1 - 6,2 - 7,4 - 9,6

-- Ways to 9 are get to 6 and jump 3 or get to 7 and jump 2

-- 0146  9
-- 01456 9

-- 0145679
-- 01467 9
-- 0147  9
-- 01457 9