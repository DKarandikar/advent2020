module Day13 where

import Utils (split )
import Math.NumberTheory.Moduli.Chinese ( chineseRemainder )

day13part1 :: IO ()
day13part1 = do
    input <- readFile "input/day13input.txt"
    let t = read (head $ lines input) :: Int
    let buses = split (==',') (lines input !! 1 )
    let res = foldl (handleLine t) (100000000, 100000000) buses
    print $ uncurry (*) res

handleLine :: Int -> (Int, Int) -> String -> (Int, Int)
handleLine x valSoFar busId = 
    if busId == "x" then valSoFar else
        let 
            value = read busId :: Int
            firstTime = head $ dropWhile (<x) [a*value | a <- [1..]]
            -- bestIdSoFar = fst valSoFar
            bestValueSoFar = snd valSoFar
            difference = firstTime - x
        in 
            if difference < bestValueSoFar then 
                (value, difference)
            else
                valSoFar

day13part2 :: IO ()
day13part2 = do
    input <- readFile "input/day13input.txt"
    let buses = split (==',') (lines input !! 1 )
    let constraints = filter (/= (0,0)) $ map (getCons buses) [0..length buses - 1]
    print constraints
    
    print $ chineseRemainder constraints

getCons :: [String] -> Int -> (Integer ,Integer)
getCons buses id = 
    let 
        busID = buses !! id
    in 
        if busID == "x" then (0,0) else
            let 
                val = read busID :: Int
            in 
                (toInteger (val - id), toInteger val)
