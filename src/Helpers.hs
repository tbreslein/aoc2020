module Helpers where

import Data.List.Split (splitOn)

between :: Ord a => a -> a -> a -> Bool
between x a b = x >= a && x <= b

paragraphs :: String -> [String]
paragraphs = splitOn "\n\n"

flatParagraphs :: String -> [[String]]
flatParagraphs =  map (concatMap words . lines) . paragraphs