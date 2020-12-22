module Day22 where

import Utils ( groupLines )

type PairOfHands = ([Int], [Int])
type RecurRound = (PairOfHands, [PairOfHands])

day22part1 :: IO ()
day22part1 = do
    input <- groupLines <$> readFile "input/day22input.txt"
    let p1Hand = map (\x -> read x :: Int) (tail (head input))
    let p2Hand = map (\x -> read x :: Int) (tail (last input))

    let final = head $ dropWhile (\(x, y) -> not (null x) && not (null y)) $ iterate playRound (p1Hand, p2Hand)
    print final
    print $ if null (fst final) then calcScore (snd final) else calcScore (fst final)

day22part2 :: IO ()
day22part2 = do
    input <- groupLines <$> readFile "input/day22input.txt"
    let p1Hand = map (\x -> read x :: Int) (tail (head input))
    let p2Hand = map (\x -> read x :: Int) (tail (last input))

    let final = head $ dropWhile shouldRecurContinue $ iterate playRecurRound ((p1Hand, p2Hand), [])
    print $ if null (fst (fst final)) then calcScore (snd (fst final)) else calcScore (fst (fst final))


playRound :: PairOfHands -> PairOfHands
playRound (p1, p2) = if 
    head p1 > head p2 then 
        (tail p1 ++ [head p1, head p2], tail p2)
    else
        (tail p1, tail p2  ++ [head p2, head p1])

calcScore :: [Int] -> Int 
calcScore xs = sum [xs !! (length xs - x - 1) * (x+1) | x <- [0.. length xs - 1]]

shouldRecurContinue :: RecurRound -> Bool
shouldRecurContinue ((x, y), l) = not (null x) && not (null y)

playRecurRound :: RecurRound -> RecurRound
playRecurRound ((p1, p2), before)
    | (p1, p2) `elem` before = ((p1 ++ p2, []), []) -- P1 wins
    | head p1 <= length (tail p1) && head p2 <= length (tail p2) = 
        if playSubGame (take (head p1) (tail p1), take (head p2) (tail p2)) 
            then ((tail p1 ++ [head p1, head p2], tail p2), before ++ [(p1, p2)])
            else ((tail p1, tail p2  ++ [head p2, head p1]), before ++ [(p1, p2)])
    | head p1 > head p2 = ((tail p1 ++ [head p1, head p2], tail p2), before ++ [(p1, p2)])
    | otherwise = ((tail p1, tail p2  ++ [head p2, head p1]), before ++ [(p1, p2)])

-- True => p1 wins
playSubGame :: ([Int], [Int]) -> Bool
playSubGame (p1, p2) = (not . null . fst . fst . head) $ dropWhile shouldRecurContinue $ iterate playRecurRound ((p1, p2), [])
