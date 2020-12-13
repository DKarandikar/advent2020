module Day13 where

import Utils (split )
import CRT ( crt )

day13part1 :: IO ()
day13part1 = do
    input <- readFile "input/day13input.txt"
    let t = read (head $ lines input) :: Int
    let buses = filter (/= "x") $ split (==',') (lines input !! 1 )
    let res = foldl (handleLine t) (100000000, 100000000) buses
    print $ uncurry (*) res

handleLine :: Int -> (Int, Int) -> String -> (Int, Int)
handleLine t valSoFar busId 
    | difference < bestValueSoFar = (value, difference)
    | otherwise = valSoFar
    where
        value = read busId :: Int
        firstTime = head $ dropWhile (<t) [a*value | a <- [1..]]
        bestValueSoFar = snd valSoFar
        difference = firstTime - t
        

day13part2 :: IO ()
day13part2 = do
    input <- readFile "input/day13input.txt"
    let constraints = getContraints $ split (==',') (lines input !! 1 )
    print $ crt constraints

getContraints :: [String] -> [(Integer, Integer)]
getContraints buses = [ (toInteger (val - index), toInteger val) 
    | index <- [0..length buses - 1]
    , buses !! index /= "x"
    , let val = read (buses !! index) :: Int
    ]
