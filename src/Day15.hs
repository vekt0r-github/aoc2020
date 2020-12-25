module Day15 (main) where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type State = (Int, Int, Map Int Int)

main :: IO ()
main = do  
    contents <- readFile "input/15.txt"
    let dataList = map (read :: String -> Int) $ splitOn "," contents
    let dataLen = length dataList
    let initState = (dataList!!(dataLen-1), dataLen, makeMap 1 dataList)
    putStrLn $ show (solve 2020 initState) ++ ", " ++ show (solve 30000000 initState)

solve :: Int -> State -> Int
solve finalStep initState = let (num, _, _) = foldl' nextState initState [1..steps] in num
    where steps = let (_, initStep, _) = initState in finalStep - initStep

nextState :: State -> Int -> State
nextState (num, step, dict) _ =
    let newNum = case Map.lookup num dict of
            Just oldStep -> step - oldStep
            Nothing      -> 0
    in makeStrict (newNum, step+1, Map.insert num step dict)

makeStrict :: State -> State
makeStrict (a, b, c) = seq a (seq b (seq c (a, b, c)))

makeMap :: Int -> [Int] -> Map Int Int
makeMap _ [] = Map.empty
makeMap x (finalStep:ns) = Map.insert finalStep x $ makeMap (x+1) ns
