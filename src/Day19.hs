{-# LANGUAGE BangPatterns #-}

module Day19 (main) where

import Data.List
import Data.List.Split
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe

type Ruleset = HashMap Int (Either String [[Int]])
type ResultCache = HashMap ([Int], String) Bool

main :: IO ()
main = do  
    contents <- readFile "input/19.txt"
    let [ruleStr, targetStr] = splitOn "\n\n" contents
    let !rules1 = foldl' parseRule Map.empty $ lines ruleStr
    let !rules2 = Map.insert 8 (Right [[42], [42, 8]]) $ Map.insert 11 (Right [[42, 31], [42, 11, 31]]) rules1
    let !targets = lines targetStr
    let result1 = sum $ map fromEnum $ accepts rules1 targets
    let result2 = sum $ map fromEnum $ accepts rules2 targets
    putStrLn $ show result1 ++ ", " ++ show result2

parseRule :: Ruleset -> String -> Ruleset
parseRule partRules ruleLine = Map.insert (read key) val partRules
    where [key, rest] = splitOn ": " ruleLine
          val = if head rest == '"' then Left $ read rest
              else Right $ map (map read . splitOn " ") $ splitOn " | " rest

matchSubstring :: Ruleset -> ResultCache -> ([Int], String) -> ResultCache
matchSubstring rules !substrMap key@(inds, target)
    | length inds > length target = Map.insert key False substrMap
    | Map.member key substrMap    = substrMap
matchSubstring rules !substrMap key@([ind], target) =
    case fromJust $ Map.lookup ind rules of
        Left s -> Map.insert key (s == target) substrMap
        Right l -> 
            let subKeys = zip l $ repeat target
                newSubstrMap = foldl' (matchSubstring rules) substrMap subKeys
            in Map.insert key (any (fromJust . (`Map.lookup` newSubstrMap)) subKeys) newSubstrMap
matchSubstring rules !substrMap key@(ind:inds, target) = Map.insert key res newSubstrMap
    where n = length target
          allSplits = map (zip [[ind], inds] . (`splitPlaces` target) . (:[n])) [1..n-1]
          (res, newSubstrMap) = foldl' (matchSubstring2 rules) (False, substrMap) allSplits

matchSubstring2 :: Ruleset -> (Bool, ResultCache) -> [([Int], String)] -> (Bool, ResultCache)
matchSubstring2 _ (_, !substrMap) [] = (True, substrMap)
matchSubstring2 _ res@(True, !substrMap) _ = res
matchSubstring2 rules (False, !substrMap) (key:keys) = case Map.lookup key newSubstrMap of
    Just True  -> matchSubstring2 rules (False, newSubstrMap) keys
    Just False -> (False, newSubstrMap)
    where newSubstrMap = matchSubstring rules substrMap key

accepts :: Ruleset -> [String] -> [Bool]
accepts rules targets = map (curry (fromJust . (`Map.lookup` substrMap)) [0]) targets
    where !substrMap = foldl' (flip (curry (flip (matchSubstring rules)) [0])) Map.empty targets