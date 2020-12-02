module Day2 where

import Utils (split)

type PasswordData = (Int, Int, Char, String)

-- Parse strings of the form \d+-\d+ [a-z]: [a-z]+
parseLine :: String -> PasswordData
parseLine s = 
    let 
        ws = words s
        limits = split (=='-') (head ws)
        lowerLim = read (head limits)
        upperLim = read (limits!!1)
        c = head $ take 1 (ws!!1) -- drop the : from it
        ss = ws!!2
    in 
        (lowerLim, upperLim, c, ss)


day2part1 :: IO ()
day2part1 = do
    input <- readFile "input/day2input.txt"
    print $ length $ filter isValid $ map parseLine $ lines input


isValid ::  PasswordData -> Bool
isValid (low, high, c, s) = 
    let 
        count = length $ filter (== c) s
    in 
        (count >= low) && (count <= high)


day2part2 :: IO ()
day2part2 = do
    input <- readFile "input/day2input.txt"
    print $ length $ filter isValidPart2 $ map parseLine $ lines input


isValidPart2 ::  PasswordData -> Bool
isValidPart2 (first, second, c, s) = (s!!(first-1) == c) /= (s!!(second-1) == c)
