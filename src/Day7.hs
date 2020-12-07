module Day7 (day7_1, day7_2) where

import Data.List (nub, (\\))
import Data.List.Split (splitOn)
import qualified Data.Map as M

type Key = String
type Value = (Int, String)

-- both tasks
toTuple :: String -> (Key, [Value])
toTuple line =
    case splitOn " contain " line of
      [x, y] ->
          if takeWhile (/=' ') y == "no"
             then (stripContainer x, [])
             else (stripContainer x, getContaineeList y)
      _ -> error $ "Could not split \"" ++ line ++ "\" on \"contain\"."

stripContainer :: String -> String
stripContainer = unwords . take 2 . words

getContaineeList :: String -> [Value]
getContaineeList str = map (f . words) . splitOn ", " $ str
    where
        f (x:xs) = (read x, unwords . take 2 $ xs)
        f [] = error $ "Could not split \"" ++ str ++ "\" into multiple words."

-- Task 2 specific
day7_2 :: String -> String
day7_2 = show . countBags . M.fromList . map toTuple . lines

countBags :: M.Map Key [Value] -> Int
countBags m = go (1, "shiny gold") - 1 -- account for the shiny gold bag
    where
        go :: Value -> Int
        go (num, bag) = num + num *
            case M.lookup bag m of
                Nothing -> 0
                Just bags -> foldr (\bag' x -> x + go bag') 0 bags

-- Task 1 specific
day7_1 :: String -> String
day7_1 = show . length . getContainers . toInvMap M.empty . map toTuple . lines

-- Why I'm calling it an inverted Map:
-- just reading how the input is structured, you would naturally associate
-- a container with a list of containees. But, in order to make this a tad
-- more performant, I inverted it, i.e. a containee is now associated to
-- its containers
toInvMap :: M.Map Key [Key] -> [(Key, [Value])] -> M.Map Key [Key]
toInvMap m [] = m
toInvMap m ((k, vals):ts) = toInvMap (insInv k vals m) ts
    where
        insInv :: Key -> [Value] -> M.Map Key [Key] -> M.Map Key [Key]
        -- if v is already in the map, append k to the entry
        -- otherwise, make a new entry with k in it
        insInv _ [] m' = m'
        insInv k ((_,v):vs) m' =
            case M.lookup v m' of
              Just _ -> insInv k vs (M.adjust (k :) v m')
              Nothing -> insInv k vs (M.insert v [k] m')

-- Plan: look up the array behind "shiny gold"
-- have two arrays, one for results, one for "keys I still need to look up"
-- per recursive call, take the head of the latter array, append it to results,
-- and then append those values behind head that are not already in results.
-- that last bit can be achieved with queue ++ (newEntries \\ results)
getContainers :: M.Map Key [Key] -> [Key]
getContainers m =
    case M.lookup "shiny gold" m of
      Nothing -> error "HOW DID THIS HAPPEN?!"
      Just ls -> go m [] ls
      where
          go :: M.Map Key [Key] -> [Key] -> [Key] -> [Key]
          go _ result [] = result
          go m result (x:xs) =
              case M.lookup x m of
                Nothing -> go m (x:result) xs
                Just [""] -> go m (x:result) xs
                Just vals -> go m (x:result) (nub $ xs ++ (vals \\ result))
