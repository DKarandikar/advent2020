module Day8 where

import Control.Monad.State

data Op = Acc Int | Jmp Int | Nop Int deriving (Show, Eq)

type S = State ([Int], Int)

day8part1 :: IO ()
day8part1 = do
    input <- readFile "input/day8input.txt"
    let ls = map parseLineToOp (lines input)
    let res = execState (eval ls 0) ([], 0)
    print res

day8part2 :: IO ()
day8part2 = do
    input <- readFile "input/day8input.txt"
    let ls = lines input
    let progs = [getSwitch x (map parseLineToOp ls) | x <- [0..length ls]]
    print $ head $ dropWhile (==([], 0)) [runProg prog | prog <- progs]


parseLineToOp:: String -> Op
parseLineToOp x = 
    let 
        a = words x
        op = head a
        arg = a!!1
        argVal = read (if '+' `elem` arg then drop 1 arg else arg) :: Int
    in  
        case op of 
            "nop" -> Nop argVal
            "jmp" -> Jmp argVal
            "acc" -> Acc argVal
            _ -> error $ "Invalid progline: " ++ x


eval:: [Op] -> Int -> S Bool
eval prog line = do
    (visited, acc) <- get
    if line `elem` visited then return False else
        if line >= length prog then return True else
            case prog!!line of
                Nop _ -> do
                    put (visited ++ [line], acc)
                    eval prog (line+1)
                Jmp val -> do
                    put (visited ++ [line], acc)
                    eval prog (line + val)
                Acc val -> do
                    put (visited ++ [line], acc + val)
                    eval prog (line + 1)


runProg:: [Op] -> ([Int], Int)
runProg prog = if evalState (eval prog 0) ([], 0) then execState (eval prog 0) ([], 0) else ([], 0)


getSwitch :: Int -> [Op] -> [Op]
getSwitch n xs = let
        newVal = case xs!!n of
            Jmp val -> Nop val
            Nop val -> Jmp val
            Acc val -> Acc val
        in 
            take n xs ++ [newVal] ++ drop (n+1) xs
