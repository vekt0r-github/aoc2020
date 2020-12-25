module Day17 (main) where

import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe

type Board = Map [Int] Bool
type State = (Board, Ranges)
type Point = [Int]
type Ranges = [(Int, Int)]

main :: IO ()
main = do  
    contents <- readFile "input/17.txt"
    let board = lines contents
    let boardState1 = parseBoard 3 board
    let boardState2 = parseBoard 4 board
    putStrLn $ show (solve 6 boardState1) ++ ", " ++ show (solve 6 boardState2)

parseBoard :: Int -> [String] -> State
parseBoard dims board = (Map.fromList pairList, ranges)
    where pad v l = if length l >= dims then l else pad v (v:l) 
          zipLine (i, line) = zipWith (\j c -> (pad 0 [i,j], c == '#')) [0..] line
          pairList = concatMap zipLine $ zip [0..] board
          ranges = pad (0, 0) [(0, length board - 1), (0, length (head board) - 1)]

solve :: Int -> State -> Int
solve 0 (boardMap, ranges) = countTrue boardMap $ pointList ranges
solve n state = solve (n-1) $ nextState state

nextState :: State -> State
nextState curState@(boardMap, ranges) = (newBoardMap, newRanges)
    where newRanges = [(a-1, b+1) | (a, b) <- ranges]
          newBoardMap = foldl' (updateMap curState) boardMap $ pointList newRanges

updateMap :: State -> Board -> Point -> Board
updateMap (oldMap, ranges) partMap point = Map.insert point newVal partMap
    where curVal = inRange ranges point && fromJust (Map.lookup point oldMap)
          neighbors = [p' | p' <- pointList [(x-1, x+1) | x <- point], inRange ranges p' && (p' /= point)]
          newVal = case countTrue oldMap neighbors of 
              3 -> True
              2 -> curVal
              _ -> False

pointList :: Ranges -> [Point]
pointList [] = [[]]
pointList ((a, b):rs) = [x:p | x <- [a..b], p <- pointList rs]

countTrue :: Board -> [Point] -> Int
countTrue boardMap points = sum $ map (fromEnum . fromJust . flip Map.lookup boardMap) points

inRange :: Ranges -> Point -> Bool
inRange ranges point = all (\((a, b), c) -> a <= c && c <= b) $ zip ranges point