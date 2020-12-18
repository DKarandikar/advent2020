module Day18 where

import Text.Parsec (char, between, digit, many1, parse)
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)
import Control.Applicative (optional, (<|>), many)


day18part1 :: IO ()
day18part1 = do
    input <- readFile "input/day18input.txt"
    print $ sum [getNum $ parse expr "" (removeSpaces l) | l <- lines input]

getNum :: Either a Int -> Int 
getNum c = case c of
    Right x -> x
    Left x -> 0

removeSpaces :: String -> String
removeSpaces s = [c | c <- s, c /= ' ']


atoi :: [Char] -> Int 
atoi = foldl f 0 where 
    f s x = 10*s + digitToInt x
 
decimal :: Parser Int 
decimal = atoi <$> many1 digit

expr :: Parser Int
expr = pure eval <*> term <*> many (pure (,) <*> (char '+' <|> char '*') <*> term)

term :: Parser Int 
term = pure f <*> optional (char '-') <*> (decimal <|> between (char '(') (char ')') expr)
    where 
        f Nothing x = x 
        f _ x = negate x

eval :: Int -> [(Char,Int)] -> Int 
eval x [] = x 
eval x (('+', x'):xs) = eval (x + x') xs
eval x (('*', x'):xs) = eval (x * x') xs


day18part2 :: IO ()
day18part2 = putStrLn "TODO"