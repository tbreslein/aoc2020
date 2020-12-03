module Day3 (day3_1, day3_2) where

-- horizontal and vertical "speed"
data Slope = Slope Int Int

-- horizontal position (vertical position is not needed)
type Position = Int
type Forest = [[Int]]

process :: Position -> Slope -> Forest -> Int
process _ _ [] = 0
process pos slope@(Slope hori vert) forest@(line:_) =
    line !! pos + process nextPos slope (drop vert forest)
    where
        -- nextPos = mod (pos + hori) (length line)
        nextPos = pos + hori
            
createProcessList :: [Slope] -> [Forest -> Int]
createProcessList = (<$>) (process 0)

convert :: [Char] -> [Int]
convert = map (\x -> if x == '#' then 1 else 0)

day3_1 :: String -> String
-- day3_1 = show . product . sequence processList . map convert . lines
day3_1 = show . product . sequence processList . map (cycle . convert) . lines
    where
        processList = createProcessList [Slope 3 1]

day3_2 :: String -> String
-- day3_2 = show . product . sequence processList . map convert . lines
day3_2 = show . product . sequence processList . map (cycle . convert) . lines
    where
        processList = createProcessList
            [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
