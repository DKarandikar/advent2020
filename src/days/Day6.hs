module Day6 where

import Data.List ( group, sort )
import qualified Data.Map as M

day6part1 :: IO ()
day6part1 = do
    input <- readFile "input/day6input.txt"
    print $ sum $ map (length. rmdups) (groupLines [] (lines input))

day6part2 :: IO ()
day6part2 = do
    input <- readFile "input/day6input.txt"
    print $ sum $ map (\(x,y) -> length $ filter (\(_, b) -> b==y) (counts x)) (groupLinesCount [] (lines input))


counts :: (Ord k, Num a) => [k] -> [(k, a)]
counts input = M.toList $ M.fromListWith (+) [(c, 1) | c <- input]


groupLines :: [String] -> [String] -> [String]
groupLines soFar ls = 
    case ls of 
        [] ->  soFar
        _ -> 
            let 
                s:ss = ls
            in
                case s of 
                    "" -> soFar ++ groupLines [] ss 
                    _ -> groupLines ((if not (null soFar) then head soFar ++ s else s) : drop 1 soFar) ss


groupLinesCount :: [(String, Int)] -> [String] -> [(String, Int)]
groupLinesCount soFar ls = 
    case ls of 
        [] ->  soFar
        _ -> 
            let 
                s:ss = ls
            in
                case s of 
                    "" -> soFar ++ groupLinesCount [] ss 
                    _ -> 
                        let 
                            (s', c) = if not (null soFar) then head soFar else ("", 0)
                        in 
                            groupLinesCount ((s' ++ s, c + 1) : drop 1 soFar) ss



rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort