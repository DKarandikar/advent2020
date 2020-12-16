module Day16 where

import Data.List ( nub, sortBy)
import Utils ( split, removeItem)

myTicket :: [Int]
myTicket = [101,71,193,97,131,179,73,53,79,67,181,89,191,137,163,83,139,127,59,61]

type Rule = ((Int, Int), (Int, Int))

day16part1 :: IO ()
day16part1 = do
    input <- readFile "input/day16rules.txt"
    nearby <- readFile "input/day16nearby.txt"
    let rules = map readRuleLine $ lines input
    let validValues = nub $ concatMap getValidValues rules
    let invalid = concatMap (getInvalid validValues) (lines nearby)
    print $ sum invalid


day16part2 :: IO ()
day16part2 = do
    input <- readFile "input/day16rules.txt"
    nearby <- readFile "input/day16nearby.txt"
    let rules = map readRuleLine $ lines input
    let validValues = nub $ concatMap getValidValues rules
    let validTickets = filter (isInvalid validValues) (lines nearby)
    let validInts = [map (\x -> read x :: Int) $ split (==',') s | s <- validTickets] ++ [myTicket]
    let locations = [[ticket !! y | ticket <- validInts] | y <- [0..19]]
    let validRulesInLocation = [getSatisfiedRuleNumbers rules (locations !! x) | x <- [0..19]]
    let ruleLocs = foldl removeDefiniteRules validRulesInLocation [0..19]
    
    let departureLocations = filter ((`elem` [0, 1, 2, 3, 4, 5]) . head . (ruleLocs !!)) [0..19] 
    print ruleLocs
    print $ product [myTicket !! x | x <- departureLocations]


readRuleLine :: String -> Rule
readRuleLine s = 
    let 
        ws = words $ split (==':') s !! 1
        part1 = map (\x -> read x :: Int) $ split (== '-') $ head ws
        part2 = map (\x -> read x :: Int) $ split (== '-') $ last ws
    in 
        ((head part1, last part1), (head part2, last part2))


getValidValues :: Rule -> [Int]
getValidValues r = [fst $ fst r .. snd $ fst r] ++ [fst $ snd r .. snd $ snd r]

getInvalid :: [Int] -> String -> [Int]
getInvalid v s = 
    let 
        values = map (\x -> read x :: Int) $ split (==',') s
    in 
        filter (not . (`elem` v)) values


removeDefiniteRules :: [[Int]] -> Int -> [[Int]]
removeDefiniteRules rules number = 
    let 
        singleRules = filter ((==1) .length) rules
        numbers = map head singleRules
    in
        map (\x -> if length x == 1 then x else filter (not . (`elem` numbers)) x) rules


isInvalid :: [Int] -> String -> Bool
isInvalid v s = null $ getInvalid v s 

getSatisfiedRuleNumbers :: [Rule] -> [Int] -> [Int]
getSatisfiedRuleNumbers rs xs = filter (checkNumbersForRule xs . (rs !!)) [0..19]
    
checkNumbersForRule :: [Int] -> Rule -> Bool 
checkNumbersForRule xs rule = all (`elem` getValidValues rule) xs
