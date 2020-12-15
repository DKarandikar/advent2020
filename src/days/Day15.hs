module Day15 where

import qualified Data.Map as M
import Data.List ( foldl' )

day15part1 :: IO ()
day15part1 = do
    input <- readFile "input/day15input.txt"
    let nums = map (\x -> read x :: Int) (lines input)
    print $ getNthSpokenWord 2020 nums


day15part2 :: IO ()
day15part2 = do
    input <- readFile "input/day15input.txt"
    let nums = map (\x -> read x :: Int) (lines input)
    print $ getNthSpokenWord 30000000 nums


getNthSpokenWord :: Int -> [Int] -> Int
getNthSpokenWord x as = M.findWithDefault (-1) (x-1) $ invert resDict
    where resDict = fst $ foldl' getNextNumber (getMap as, last as) [length as - 1..x]


invert :: M.Map Int Int -> M.Map Int Int
invert m = M.fromList [(v, k) | k <- M.keys m, let v = M.findWithDefault (-1) k m]


getMap :: [Int] -> M.Map Int Int 
getMap as = M.fromList $ zip (init as) [0..length as - 1]


getNextNumber:: (M.Map Int Int, Int) -> Int -> (M.Map Int Int, Int)
getNextNumber (m, mostRecent) a = 
    case M.lookup mostRecent m of
        Just loc -> (M.insert mostRecent a m, a - loc)
        Nothing -> (M.insert mostRecent a m, 0)
