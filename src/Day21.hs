{-# LANGUAGE BangPatterns #-}

module Day21 (main) where

import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Day16

type Food = ([String], [String])
type Options = Map String (Set String)

main :: IO ()
main = do  
    contents <- readFile "input/21.txt"
    let !foods = map parseLine $ lines contents
    let !(allIngs, allAlrs) = 
            let (foodIngs, foodAlrs) = unzip foods
            in (distinctElems foodIngs, distinctElems foodAlrs)
    let !initOptions = Map.fromList $ zip (Set.toList allAlrs) $ repeat allIngs
    let !options = foldl' matchFood initOptions foods
    let !safeIngs = Map.foldl' (Set.\\) allIngs options
    let !alrIngs = allIngs Set.\\ safeIngs
    putStrLn $ show (solve1 safeIngs foods) ++ ", " ++ solve2 options (alrIngs, allAlrs)

parseLine :: String -> Food
parseLine line = (words ingStr, splitOn ", " $ init alrStr)
    where [ingStr, alrStr] = splitOn " (contains " line

distinctElems :: (Ord a) => [[a]] -> Set a
distinctElems = foldr (Set.union . Set.fromList) Set.empty

matchFood :: Options -> Food -> Options
matchFood partOptions (_, []) = partOptions
matchFood partOptions (ings, alr:alrs) = matchFood options (ings, alrs)
    where alrOptions = Set.intersection (partOptions Map.! alr) $ Set.fromList ings
          options = Map.insert alr alrOptions partOptions

solve1 :: Set String -> [Food] -> Int
solve1 safeIngs foods = sum $ map (fromEnum . (`Set.member` safeIngs)) $ concatMap fst foods

solve2 :: Options -> (Set String, Set String) -> String
solve2 options (ings, alrs) = intercalate "," $ map (perm Map.!) (Set.elems alrs)
    where optionsFunc alr = (`Set.member` (options Map.! alr))
          perm = Day16.findPerm optionsFunc (Set.toList alrs) (Set.toList ings)