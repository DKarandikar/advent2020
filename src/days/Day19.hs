module Day19 where

import qualified Data.Map as M 
import qualified Data.Set as S

import Utils ( group )

type CompleteRule = [String] 
type IncompleteRule = ([Int], Maybe [Int])

type IncompleteDict = M.Map Int IncompleteRule
type CompleteDict = M.Map Int CompleteRule


day19part1 :: IO ()
day19part1 = do
    input <- lines <$> readFile "input/day19rules.txt"
    let ruleDict = fst $ head $ dropWhile (not . M.null . snd) $ iterate reduceRules $ parseInput input

    messages <- lines <$> readFile "input/day19messages.txt"
    print $ length [x | x <- messages, x `elem` M.findWithDefault [] 0 ruleDict]


parseInput:: [String] -> (CompleteDict, IncompleteDict)
parseInput = foldl handleLine (M.empty, M.empty)

handleLine :: (CompleteDict, IncompleteDict) -> String -> (CompleteDict, IncompleteDict)
handleLine (c, i) s = 
    let 
        ws = words s
        incomplete = '"' `notElem` s
        number = read (init (head ws)) :: Int
        firstChunk = takeWhile (/= "|") (tail ws)
        secondChunk = dropWhile (/= "|") (tail ws)
        firstBitOfRule = [read x :: Int | x <- firstChunk]
        secondBitOfRule = if null secondChunk then Nothing else Just [read x :: Int | x <- tail secondChunk]
    in 
        if incomplete then 
            (c, M.insert number (firstBitOfRule, secondBitOfRule) i) 
        else 
            (M.insert number [[c | c <- ws!!1, c /= '"']] c, i)


reduceRules :: (CompleteDict, IncompleteDict) -> (CompleteDict, IncompleteDict)
reduceRules (c, i) = foldl reduceKeyIfPossible (c, i) (M.keys i)

reduceKeyIfPossible :: (CompleteDict, IncompleteDict) -> Int -> (CompleteDict, IncompleteDict)
reduceKeyIfPossible (c, i) key = 
    let 
        rule = case M.lookup key i of 
            Just x -> x
            Nothing -> error "Not found key"
        ruleValues = case snd rule of 
            Just vals -> fst rule ++ vals
            Nothing -> fst rule
        canSubstitue = all (`elem` M.keys c) ruleValues
        val = case snd rule of 
            Nothing -> [concat y | y <- sequenceA [M.findWithDefault ["ERROR"] x c | x <- fst rule]]
            Just vals -> [concat y | y <- sequenceA [M.findWithDefault ["ERROR"] x c | x <- fst rule]] ++ [concat y | y <- sequenceA [M.findWithDefault ["ERROR"] x c | x <- vals]]
    in
        if canSubstitue then
            (M.insert key val c, M.delete key i)
        else
            (c, i)


day19part2 :: IO ()
day19part2 = do
    input <- lines <$> readFile "input/day19rulesPart2.txt"
    let (complete, incomplete) = parseInput input
    print (complete, incomplete)
    let ruleDict = head $ dropWhile ((/=3) . M.size . snd) $ iterate reduceRules (complete, incomplete)

    -- (2+) 42s then (1+) 31s ,, also they're all length 8
    let rule42 = M.findWithDefault [] 42 (fst ruleDict)
    let rule31 = M.findWithDefault [] 31 (fst ruleDict)

    messages <- lines <$> readFile "input/day19messages.txt"
    print $ length $ filter (matchLine rule42 rule31) messages


matchLine :: [String] -> [String] -> String -> Bool 
matchLine rule42 rule31 s =
    let 
        bunches = group (length $ head rule42) s
        startBunches = takeWhile (`elem` rule42) bunches
        endBunches = dropWhile (`elem` rule42) bunches

    in 
        length startBunches >= length endBunches + 1 && length startBunches >= 2 && not (null endBunches) && all (`elem` rule31) endBunches 
