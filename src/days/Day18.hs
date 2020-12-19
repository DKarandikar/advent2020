module Day18 where

import Text.Parsec (char, between, digit, many1, parse)
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)
import Control.Applicative (optional, (<|>), many)


day18part1 :: IO ()
day18part1 = do
    input <- lines . removeSpaces <$> readFile "input/day18input.txt"
    print $ sum [getNum $ parse expr "" l | l <- input]

day18part2 :: IO ()
day18part2 = do
    input <- lines . removeSpaces <$> readFile "input/day18input.txt"
    print $ sum [getNum $ parse expr2 "" l | l <- input]


getNum :: Either a Int -> Int 
getNum c = case c of
    Right x -> x
    Left x -> 0

removeSpaces :: String -> String
removeSpaces s = [c | c <- s, c /= ' ']

num :: Parser Int 
num = read <$> many1 digit

expr :: Parser Int
expr = eval <$> term <*> many ((,) <$> (char '+' <|> char '*') <*> term)

term :: Parser Int 
term = num <|> between (char '(') (char ')') expr

expr2 :: Parser Int
expr2 = eval <$> timesexpr <*> many ((,) <$> char '*' <*> timesexpr)

timesexpr :: Parser Int
timesexpr = eval <$> term2 <*> many ((,) <$> char '+' <*> term2)

term2 :: Parser Int 
term2 = num <|> between (char '(') (char ')') expr2

eval :: Int -> [(Char,Int)] -> Int 
eval x [] = x 
eval x (('+', x'):xs) = eval (x + x') xs  -- for right-assoc do x + eval x' xs
eval x (('*', x'):xs) = eval (x * x') xs
