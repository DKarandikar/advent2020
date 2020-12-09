module Day9 where

preable :: Int
preable = 25

day9part1 :: IO ()
day9part1 = do
    input <- readFile "input/day9input.txt"
    let ls = map (\x -> read x :: Int) (lines input)
    print $ getInvalidNumber ls

day9part2 :: IO ()
day9part2 = do
    input <- readFile "input/day9input.txt"
    let ls = map (\x -> read x :: Int) (lines input)
    print $ doDay9Part2 ls


getInvalidNumber:: [Int] -> Int
getInvalidNumber ls = ls!!(preable + head (dropWhile (isValid ls) [0..length ls]))

isValid:: [Int] -> Int ->  Bool
isValid nums x = (nums!!(x+preable)) `elem` sums 
    where sums = [a + y | a <- take preable (drop x nums), y <- take preable (drop x nums), x /=y ]

doDay9Part2 :: [Int] -> Int
doDay9Part2 ls = 
    let 
        invalid = getInvalidNumber ls
        start = head $ dropWhile (\x -> null (findRange ls x invalid)) [0.. length ls]
        numbers = length $ findRange ls start invalid
        contiguousRange = take (numbers-1) (drop start ls)
    in
        maximum contiguousRange + minimum contiguousRange

findRange:: [Int] -> Int -> Int -> [Int]
findRange input loc x = 
    let 
        nums = drop loc input
        sums = takeWhile (<=x) [sum (take y nums)| y <- [0..]]
    in
        if last sums == x then sums else []