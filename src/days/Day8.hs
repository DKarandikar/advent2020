module Day8 where

import Control.Monad.State

type S = State ([Int], Int)

day8part1 :: IO ()
day8part1 = do
    input <- readFile "input/day8input.txt"
    let ls = lines input
    let res = execState (eval ls 0) ([], 0)
    print $ fst res
    print $ snd res


eval:: [String] -> Int -> S Bool
eval prog line = do
    let nextLine = line
    (visited, acc) <- get

    if line `elem` visited then return False else
        if line >= length prog then return True else
            let 
                a = words (prog!!nextLine)
                op = head a
                arg = a!!1
            in  
                case op of
                    "nop" -> do
                        put (visited ++ [line], acc)
                        eval prog (line+1)
                    "jmp" -> do
                        put (visited ++ [line], acc)
                        eval prog (line + read (if '+' `elem` arg then drop 1 arg else arg))
                    "acc" -> do
                        put (visited ++ [line], acc + read (if '+' `elem` arg then drop 1 arg else arg))
                        eval prog (line + 1)
                    _ -> error ""


day8part2 :: IO ()
day8part2 = do
    input <- readFile "input/day8input.txt"
    let ls = lines input
    let progs = [getSwitch x ls | x <- [0..length ls]]
    print $ [runProg prog | prog <- progs]

runProg:: [String] -> ([Int], Int)
runProg prog = if evalState (eval prog 0) ([], 0) then execState (eval prog 0) ([], 0) else ([], 0)


getSwitch :: Int -> [String] -> [String]
getSwitch _ [] = []
getSwitch n (x:xs)
    | n == 0 = 
        let 
            a = words x
            op = head a
            arg = a!!1
        in 
            case op of
                "jmp" -> ("nop" ++ " " ++ arg):xs
                "nop" -> ("jmp" ++ " " ++ arg):xs
                _ -> x:xs

    | otherwise = x:getSwitch (n-1) xs