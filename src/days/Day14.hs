module Day14 where

import Data.Bits ( Bits(setBit, testBit) )
import Data.List ( foldl' )
import qualified Data.Map as M 


day14part1 :: IO ()
day14part1 = do
    input <- readFile "input/day14input.txt"
    let res = foldl handleLine ("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", M.empty) (lines input)
    print $ sum [M.findWithDefault 0 key (snd res) | key <- M.keys (snd res)]

day14part2 :: IO ()
day14part2 = do
    input <- readFile "input/day14input.txt"
    let res = foldl handleLine2 ("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", M.empty) (lines input)
    print $ sum [M.findWithDefault 0 key (snd res) | key <- M.keys (snd res)]


convertToBits :: Int -> [Bool] 
convertToBits x = map (testBit x) [35,34..0]

convertFromBits :: [Bool] -> Int
convertFromBits bs = foldl' (\acc (i,b) -> if b then setBit acc i else acc) 0 $ zip [35, 34..0] bs


applyMask :: String -> [Bool] -> [Bool]
applyMask s num = [(s !! index == '1') || (((s !! index) /= '0') && (num !! index)) | index <- [0..35]]


handleLine :: (String, M.Map Int Int) -> String -> (String, M.Map Int Int)
handleLine (mask, m) line = 
    if line!!1 == 'a' then (words line !! 2, m) else
        let 
            ws = words line
            memLoc = read (init (drop 4 (head ws))) :: Int
            number = read (ws!!2) :: Int
            bits = convertToBits number
            res = convertFromBits $ applyMask mask bits
        in 
            (mask, M.insert memLoc res m)


data X = T | F | X deriving (Show, Eq)

-- Takes a mask and a number, and returns all disambiguations of Xs in the mask to [Bool] 
applyMask2 :: String -> [Bool] -> [[Bool]]
applyMask2 s num = foldl genCombo [[]] p
    where
        p = [
            if s!!index == '1' then T else 
                (if s!!index == 'X' then X else 
                    (if num!!index then T else F)
                ) 
            | index <- [0..35]]

genCombo :: [[Bool]] -> X -> [[Bool]]
genCombo ss x = if x /= X then [cs ++ [x == T] | cs <- ss] else [cs  ++ [y] | cs <- ss, y <- [False, True]]


handleLine2 :: (String, M.Map Int Int) -> String -> (String, M.Map Int Int)
handleLine2 (mask, m) line = 
    if line!!1 == 'a' then (words line !! 2, m) else
        let 
            ws = words line
            memLoc = read (init (drop 4 (head ws))) :: Int
            number = read (ws!!2) :: Int
            locations = map convertFromBits (applyMask2 mask $ convertToBits memLoc)
            resMap = foldr (M.union. M.fromList) m [[(loc, number)] | loc <- locations]
        in 
            (mask, resMap)
