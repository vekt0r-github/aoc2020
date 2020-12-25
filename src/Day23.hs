{-# LANGUAGE BangPatterns #-}

module Day23 (main) where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe

type Next = Map Int Int
type State = (Int, Next)

main :: IO ()
main = do  
    contents <- readFile "input/23.txt"
    let !input = map read $ chunksOf 1 contents
    let !initState1 = makeState input
    let !initState2 = let (x, nextDict) = initState1 in
            (x, Map.insert (last input) 10 $ Map.insert (10^6) (head input) nextDict)
    let result1 = concatMap show $ fst $ pickUp 1 (9-1) $ snd $ simulate 9 100 initState1
    let result2 = product $ fst $ pickUp 1 2 $ snd $ simulate (10^6) (10^7) initState2
    putStrLn $ result1 ++ ", " ++ show result2

makeState :: [Int] -> State
makeState cups = (head cups, nextDict)
    where nextDict = Map.fromList $ zip ((last cups):cups) cups

getOrDefault :: (Ord a) => (a -> b) -> Map a b -> a -> b
getOrDefault def dict label = case Map.lookup label dict of
    Just newLabel -> newLabel
    Nothing       -> def label

next :: (Ord a, Enum a) => Map a a -> a -> a
next = getOrDefault succ

prevLabelWithSize :: Int -> [Int] -> Int -> Int
prevLabelWithSize maxVal picked 0 = prevLabelWithSize maxVal picked maxVal
prevLabelWithSize maxVal picked cur = if cur `notElem` picked then cur
    else prevLabelWithSize maxVal picked (cur-1)
    
simulate :: Int -> Int -> State -> State
simulate maxVal 0 !state = state
simulate maxVal t !state = simulate maxVal (t-1) $ nextState (prevLabelWithSize maxVal) state

nextState :: ([Int] -> Int -> Int) -> State -> State
nextState prevLabel (!cur, !nextDict) = (next newNextDict cur, newNextDict)
    where (picked, halfNewNextDict) = pickUp cur 3 nextDict
          preCur = prevLabel picked (cur-1)
          newNextDict = putDown preCur picked halfNewNextDict

pickUp :: Int -> Int -> Next -> ([Int], Next)
pickUp loc len !nextDict = (picked, newNextDict)
    where picked = take len $ tail $ iterate' (next nextDict) loc
          newNextDict = Map.insert loc (next nextDict $ last picked) nextDict

putDown :: Int -> [Int] -> Next -> Next
putDown loc picked !nextDict = newNextDict
    where halfNewNextDict = Map.insert (last picked) (next nextDict loc) nextDict
          newNextDict = Map.insert loc (head picked) halfNewNextDict