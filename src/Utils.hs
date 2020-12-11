module Utils where

import Data.List ( group, sort )
import qualified Data.Map as M


-- Split string on all chars that satisfy (Char -> Bool)
split:: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'


-- Get the last N of a list
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs


-- Get all but the last N of a list
firstMinusN :: Int -> [a] -> [a]
firstMinusN n xs = take (length xs - n) xs


-- Remove an item from a list
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


-- Get a list of k:count which counts the frequency of each k in [k]
counts :: (Ord k, Num a) => [k] -> [(k, a)]
counts input = M.toList $ M.fromListWith (+) [(c, 1) | c <- input]


-- Remove duplicates from input in O(nlogn)
removeDups :: (Ord a) => [a] -> [a]
removeDups = map head . group . sort


-- 
groupLines :: String -> [[String]]
groupLines i = groupLines' [] (lines i)

groupLines' :: [[String]] -> [String] -> [[String]]
groupLines' soFar ls = 
    case ls of 
        [] ->  soFar
        _ -> 
            let 
                s:ss = ls
            in
                case s of 
                    "" -> soFar ++ groupLines' [] ss 
                    _ -> groupLines' ((if not (null soFar) then head soFar ++ [s] else [s]) : drop 1 soFar) ss


converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

-- Apply f on a iteratively until f^(n+1)(a) = f^(n)(a)
runTilEqual:: Eq a => (a -> a) -> a -> a
runTilEqual f = converge (==) . iterate f