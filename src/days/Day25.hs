module Day25 where

import Math.NumberTheory.Powers.Modular (powModInt )
import Data.List (iterate')

test1 :: Int 
test1 = 5764801
test2 :: Int 
test2 = 17807724

key1 :: Int
key1 = 9717666
key2 :: Int
key2 = 20089533


day25part1 :: IO ()
day25part1 = do
    let loopSize2 = head $ filter ((== key2) . snd) $ [(x, powModInt  7 x 20201227) | x <- [3..]]
    print $ doTrans key1 (fst loopSize2)

doTrans :: Int -> Int -> Int 
doTrans subject loopSize = iterate' (transform subject) 1 !! loopSize

transform :: Int -> Int -> Int 
transform subject val = (val * subject) `rem` 20201227
