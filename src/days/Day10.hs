module Day10 where

import Utils ( counts )
import Data.List ( group, sort )

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
    let sorted = sort ls
    let groups = group $ zipWith (-) (tail sorted) sorted

    -- The input is easier than the problem as stated, there are no diffs of 2
    -- Runs of 3 are boring, there's one path through them
    -- Runs of 1 can be reordered according to the tribonacci number of their length

    let run1Perms = map ((tribonacci!!) . length) $ filter (\(x:_) -> x == 1) groups

    print $ product run1Perms


tribonacci:: [Integer]
tribonacci = 1 : 1 : 2 : zipWith3 (\x y z -> x+y+z) tribonacci (tail tribonacci) (tail $ tail tribonacci)
   

headOrNo :: Num p => [p] -> Int -> p
headOrNo cs z = 
    let a = drop z cs in
    if not (null a) then head a else 0