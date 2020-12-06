module Day6 where

import Utils (groupLines, removeDups, counts)


day6part1 :: IO ()
day6part1 = do
    input <- readFile "input/day6input.txt"
    print $ sum $ map (length. removeDups . concat) (groupLines input)


day6part2 :: IO ()
day6part2 = do
    input <- readFile "input/day6input.txt"
    print $ sum $ map getCountAllAnswered (groupLines input)


-- Filter for only chars that appear on every line, and get the count of those
-- This assumes we don't have any invalid lines, i.e. only single occurrences of any char per line
getCountAllAnswered :: [String] -> Int
getCountAllAnswered lines = length $ filter (\(_, b) -> b == length lines) (counts $ concat lines)
