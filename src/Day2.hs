module Day2
    ( day2_1
    , day2_2
    )
    where

import Data.List.Split (splitOn)
import Data.Algebra.Boolean (xor)

data PassLine = PassLine Int Int Char String deriving (Show)

checkPassLine1 :: PassLine -> Int
checkPassLine1 (PassLine minNum maxNum letter password) =
    let count = length $ filter (== letter) password
     in if count >= minNum && count <= maxNum then 1 else 0

checkPassLine2 :: PassLine -> Int
checkPassLine2 (PassLine minNum maxNum letter password)
    -- this first guard is superfluous, because the password lengths are well formed, but I'm gonna
    -- leave this here
    | length password < maxNum || length password < minNum = 0
    | xor (password !! (minNum-1) == letter) (password !! (maxNum-1) == letter) = 1
    | otherwise = 0

-- this obviously assumes perfect input... it's lazy, but the AoC input can safely be assumed to be
-- perfectly well formed. If this was "production code", I should probably have an error sum type,
-- have this function use the Either monad (something like Either PassLineErr Passline)
parse :: String -> PassLine
parse line = let [numRange, letter, password] = words line
                 [x,y] = splitOn "-" numRange
              in PassLine (read x) (read y) (head letter) password

process :: (PassLine -> Int) -> [PassLine] -> Int
process f = foldr ((+) . f) 0

day2_1 :: String -> String
day2_1 = show . process checkPassLine1 . map parse . lines

day2_2 :: String -> String
day2_2 = show . process checkPassLine2 . map parse . lines