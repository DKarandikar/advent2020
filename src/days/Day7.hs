module Day7 where

import qualified Data.Map as M

type BagDesc = String
type BagMap = M.Map BagDesc [(Int, BagDesc)]


day7part1 :: IO ()
day7part1 = do
    input <- readFile "input/day7input.txt"
    let bagMap = getBagMap input
    print $ length $ filter (containsBagEventually bagMap "shiny gold") (M.keys bagMap)


day7part2 :: IO ()
day7part2 = do
    input <- readFile "input/day7input.txt"
    print $ countBags (getBagMap input) "shiny gold"


getBagMap :: String -> BagMap
getBagMap input = foldr (M.union. handleLine) M.empty (lines input)


-- adjective colour bags contain [number adjective colour bags] or "no other bags"
handleLine:: String -> BagMap
handleLine s = 
    let 
        ws = words s
        bagName = head ws ++ " " ++ ws!!1
        subbags = getBags (drop 4 ws)
    in 
        M.fromList [(bagName, subbags)]

getBags:: [String] -> [(Int, BagDesc)]
getBags s = if s == ["no", "other", "bags."] then [] else 
    let (num:adjective:colour:bags:rest) = s in
        (read num, adjective ++ " " ++ colour) : if last bags == ',' then getBags rest else []


containsBagEventually:: BagMap -> BagDesc -> BagDesc -> Bool
containsBagEventually bagMap toFind bag = 
    let 
        thisBag = M.findWithDefault [] bag bagMap
        inThisBag = toFind `elem` map snd thisBag
        inASubBag = any (containsBagEventually bagMap toFind . snd) thisBag
    in 
        inThisBag || inASubBag


-- It's X contains [N blahs], .. so add up all the bags + N * their value
countBags :: BagMap -> BagDesc -> Int
countBags bagMap topBag = 
    let 
        bags = M.findWithDefault [] topBag bagMap
        topLevelBags = map fst bags
        subBagCounts = map (countBags bagMap . snd) bags
        topLevelBagsCount = sum topLevelBags
        subBagsCount = sum (zipWith (*) topLevelBags subBagCounts)
    in 
        topLevelBagsCount + subBagsCount
