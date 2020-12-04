module Day4 where

import Utils

type Passport = [String]

day4part1 :: IO ()
day4part1 = do
    input <- readFile "input/day4input.txt"
    print $ length $ filter checkNumKeys (getPassports input)


day4part2 :: IO ()
day4part2 = do
    input <- readFile "input/day4input.txt"
    print $ length $ filter checkValidPassport (getPassports input)


getPassports :: String -> [Passport]
getPassports i =  accumWords [] (lines i)

accumWords :: [String] -> [String] -> [Passport]
accumWords soFar ls = 
    case ls of 
        [] ->  [soFar]
        _ -> 
            let 
                s:ss = ls
            in
                case s of 
                    "" -> soFar : accumWords [] ss 
                    _ -> accumWords (words s ++ soFar) ss


checkNumKeys:: Passport -> Bool
checkNumKeys passport = check $ map (head . split (==':')) passport

check:: [String] -> Bool
check ss = elem "byr" ss && elem "iyr" ss && elem "eyr" ss && elem "hgt" ss && elem "hcl" ss && elem "ecl" ss && elem "pid" ss


checkValidPassport:: Passport -> Bool
checkValidPassport passport = checkNumKeys passport && all validateKeyValue passport


validateKeyValue:: String -> Bool
validateKeyValue ss =
    let 
        a = split (==':') ss
        key = head a
        value = a!!1
    in 
        case key of
            "byr" -> (read value :: Int) >= 1920 && (read value :: Int) <= 2002
            "iyr" -> (read value :: Int) >= 2010 && (read value :: Int) <= 2020
            "eyr" -> (read value :: Int) >= 2020 && (read value :: Int) <= 2030
            "hgt" -> 
                let units = lastN 2 value in
                    case units of
                        "cm" -> (read (firstMinusN 2 value) ::Int) >= 150 && (read (firstMinusN 2 value) ::Int) <= 193
                        "in" -> (read (firstMinusN 2 value) ::Int) >= 59 && (read (firstMinusN 2 value) ::Int) <= 76
                        _ -> False
            "hcl" -> head value == '#' && all (`elem` "0123456789abcdef") (drop 1 value)
            "ecl" -> value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
            "pid" -> length value == 9 && all (`elem` "0123456789") value
            _ -> True


-- invalid
-- hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007
-- hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
-- iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946
-- eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:192
