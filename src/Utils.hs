module Utils where

-- Split string on all chars that satisfy (Char -> Bool)
split:: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

-- Get the last N of a list
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- Get all but the last N of a list
firstMinusN :: Int -> [a] -> [a]
firstMinusN n xs = take (length xs - n) xs
