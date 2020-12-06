module Day4 (day4_1, day4_2) where

import Helpers (between, flatParagraphs)
import Data.List (isSuffixOf, (\\))
import Data.List.Split (splitOn)

-- Constants
mustHaves :: [String]
mustHaves = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hexDigits :: [Char]
hexDigits = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

allFields :: [String]
allFields = "cid" : mustHaves

lengthMustHaves :: Int
lengthMustHaves = length mustHaves

lengthAllFields :: Int
lengthAllFields = lengthMustHaves + 1

-- exported functions
day4_1 :: String -> String
day4_1 = processInput f
    where f _ = 1

day4_2 :: String -> String
day4_2 = processInput f
    where f passports = if all validateFields passports then 1 else 0

-- everything else
processInput :: ([String] -> Int) -> String -> String
processInput f =
    show . sum . map (processPassport f) . flatParagraphs

processPassport :: ([String] -> Int) -> [String] -> Int
processPassport _ [] = 0
processPassport f s
    | length s == lengthMustHaves = if checkMustHaveFields s then f s else 0
    | length s == lengthAllFields = if checkAllFields s then f s else 0
    | otherwise = 0
    where
        -- naming is weird, I know. These just check wether there are any fields
        -- that are not in mustHaves or allFields respectively,
        -- because if so, we know the passport is invalid right away
        checkMustHaveFields s = null $ mustHaves \\ map (head . splitOn ":") s
        checkAllFields s = null $ allFields \\ map (head . splitOn ":") s

-- specific to Task 2
validateFields :: String -> Bool
validateFields s = case splitOn ":" s of
    ["byr", x] ->
        length x == 4 && isNumber x && let xNum = read x in between xNum 1920 2002
    ["iyr", x] ->
        length x == 4 && isNumber x && let xNum = read x in between xNum 2010 2020
    ["eyr", x] ->
        length x == 4 && isNumber x && let xNum = read x in between xNum 2020 2030
    ["hgt", x] -> checkHeight x
    ["hcl", x] -> (head x == '#') && (length (tail x) == 6) && isHexDigits (tail x)
    ["ecl", x] -> x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    ["pid", x] -> length x == 9 && all (`elem` ['0'..'9']) x
    ["cid", _] -> True
    _ -> False
    where
        isNumber = all (`elem` ['0'..'9'])
        isHexDigits = all (`elem` hexDigits)
        checkHeight x
            | "cm" `isSuffixOf` x = length x == 5 && let xNum = read (take 3 x)
                                                      in between xNum 150 193
            | "in" `isSuffixOf` x = length x == 4 && let xNum = read (take 2 x)
                                                      in between xNum 59 76
            | otherwise = False