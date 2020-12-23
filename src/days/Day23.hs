module Day23 where

import Data.List ( iterate' )
import qualified Data.Map as M

testInput :: [Int]
testInput = [3, 8, 9, 1, 2, 5, 4, 6, 7]
realInput :: [Int]
realInput = [6, 1, 4, 7, 5, 2, 8, 3, 9]

day23part1 :: IO ()
day23part1 = do
    let res = iterate doMove realInput !! 100
    let start = takeWhile (/= 1) res
    let end = tail $ dropWhile (/= 1) res
    putStrLn $ concat [show x | x <- end ++ start]

doMove :: [Int] -> [Int]
doMove vals =
    let 
        current = head vals
        toMove = take 3 (tail vals)
        remainder = drop 4 vals
        maxVal = maximum remainder
        destinationVal = head $ dropWhile (`elem` toMove) [if (current - x) > 0 then current-x else (current-x) + maxVal | x <- [1..]]
    in 
        takeWhile (/= destinationVal) remainder ++ [destinationVal] ++ toMove ++ tail (dropWhile (/= destinationVal) remainder) ++ [current]


type CircularMap = M.Map Int (Int, Int)

maxValue :: Int
maxValue = 1000000

getStartingMap :: [Int] -> CircularMap
getStartingMap t = M.fromList $ (-1, (head t, 0)) : [(t !! x, (t !! x, if (x+1) < 9 then t !! (x+1) else 10)) | x <- [0..8]] ++ [(x, (x, if x < maxValue then x+1 else head t)) | x <- [10..maxValue]]

day23part2 :: IO ()
day23part2 = do
    let res = iterate' doMoveMap (getStartingMap realInput) !! 10000000
    let two = lookupOrError (snd $ lookupOrError 1 res) res

    print$ uncurry (*) two


doMoveMap :: CircularMap -> CircularMap
doMoveMap m = 
    let
        currentHead = lookupOrError (fst $ lookupOrError (-1) m) m
        current = fst currentHead
        val2 = lookupOrError (snd currentHead) m
        val3 = lookupOrError (snd val2) m
        val4 = lookupOrError (snd val3) m

        removedVals = [fst x | x <- [val2, val3, val4]]

        destinationVal = head $ dropWhile (`elem` removedVals) [if (current -x) > 0 then current -x else (current - x) + maxValue | x <- [1..]]
        dest = lookupOrError destinationVal m

        -- Set -1 for the new head, which is right of val4
        m' = M.insert (-1) (snd val4, 0) m

        -- Rewire right of current head to be right of val4
        m'' = M.insert current (current, snd val4) m'
        -- Right of val4 to be right of dest
        m''' = M.insert (fst val4) (fst val4, snd dest) m''
    in 
        -- Right of dest to be val2
        M.insert (fst dest) (fst dest, fst val2) m'''


lookupOrError ::  Int -> CircularMap -> (Int, Int)
lookupOrError k m = case M.lookup k m of
    Just x -> x
    Nothing -> error "Not found"
