module Day21 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( intercalate )

import Utils ( split, removeItem, counts )


type AlergenMap = M.Map String [S.Set String]
type AlergenNamingMap = M.Map String [String]
type AlergenNames = M.Map String String


day21part1 :: IO ()
day21part1 = do
    input <- lines <$> readFile "input/day21input.txt"
    let alergenMap = foldr (combine. parseLine) M.empty input
    let allIngredients = foldl S.union S.empty (concat $ M.elems alergenMap)
    let unsafe = foldl S.union S.empty (map getUnSafe $ M.elems alergenMap)
    let safeIngredients = S.difference allIngredients unsafe
    let totalIngredientsCount = M.fromList $ counts $ concatMap getIngredients input

    print $ sum [M.findWithDefault 0 k totalIngredientsCount | k <- S.toList safeIngredients]


getUnSafe :: [S.Set String] -> S.Set String
getUnSafe = foldl1 S.intersection

combine :: AlergenMap -> AlergenMap -> AlergenMap
combine m m2 = foldr add m (M.keys m2)
    where add  key m' = case M.lookup key m' of
            Just x -> M.insert key (x ++ M.findWithDefault [] key m2) m'
            Nothing -> M.insert key (M.findWithDefault [] key m2) m'

getIngredients :: String -> [String]
getIngredients ss = words (head (split (=='(') ss))

parseLine :: String -> AlergenMap
parseLine ss = 
    let 
        parts = split (=='(') ss
        ingredients = words (head parts)
        alergens = tail $ words $ removeItem ',' $ init (parts !! 1)
    in 
        foldl (\m x -> M.insert x [S.fromList ingredients] m) M.empty alergens

day21part2 :: IO ()
day21part2 = do
    input <- lines <$> readFile "input/day21input.txt"
    let alergenMap = foldr (combine. parseLine) M.empty input

    let unsafeDict = M.fromList [(x, S.toList $ getUnSafe (M.findWithDefault [S.empty] x alergenMap)) | x <- M.keys alergenMap]
    let allergenNames = M.toList $ fst $ head $ dropWhile (not . null. snd) $ iterate reduce (M.empty, unsafeDict) 

    putStrLn $ intercalate "," $ map snd allergenNames

reduce :: (AlergenNames, AlergenNamingMap) -> (AlergenNames, AlergenNamingMap)
reduce (c, i) = foldl reduceKeyIfPossible (c, i) (M.keys i)


reduceKeyIfPossible :: (AlergenNames, AlergenNamingMap) -> String -> (AlergenNames, AlergenNamingMap)
reduceKeyIfPossible (c, i) key = 
    let 
        alergens = case M.lookup key i of 
            Just x -> x
            Nothing -> error "Not found key"
        
        couldBe = foldr removeItem alergens (M.elems c)

    in 
        if length couldBe == 1 then 
            (M.insert key (head couldBe) c, M.delete key i)
        else
            (c, i)