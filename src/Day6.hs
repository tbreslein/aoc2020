module Day6 (day6_1, day6_2) where

import Helpers (flatParagraphs, paragraphs)
import Data.List (intersect, nub)

-- | Given an input with multiple paragraphs, this function collects
-- the unique letters per paragraph, and then measures the per-paragraph
-- over the whole input string
day6_1 :: String -> String
day6_1 = show . length . concatMap (nub . concat) . flatParagraphs

-- | Given an input with multiple paragraphs, this function takes each
-- paragraph and measures the length of the intersection between each
-- lines of that paragraph, and then sums up the length of those
-- per-paragraph intersections
day6_2 :: String -> String
day6_2 = show . foldr ((+) . length . process . lines) 0 . paragraphs
    where
        process :: [String] -> String
        process (x:xs) = x `intersect` process xs
        -- Why this is here: "abc" `intersect` ['a'..'z'] == "abc"
        process [] = ['a'..'z']