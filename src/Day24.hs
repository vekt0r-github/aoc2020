{-# LANGUAGE BangPatterns #-}

module Day24 (main) where

import Data.Functor.Identity
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe

type Point = (Int, Int)
type State = Set Point

main :: IO ()
main = do  
    contents <- readFile "input/24.txt"
    let initState = update Set.empty $ map (moveStr (0, 0)) $ lines contents
    let finalState = simulate 100 initState
    putStrLn $ show (length initState) ++ ", " ++ show (length finalState)

dirs :: [String]
dirs = ["e", "se", "sw", "w", "nw", "ne"]

moveDir :: Point -> String -> Point
moveDir (a, b) dir = (a + dFunc 2 dirIndex, b + dFunc 0 dirIndex)
    where dirIndex = fromJust $ elemIndex dir dirs
          dFunc x ind = (1 - 2 * (ind `div` 3)) * fromEnum (ind `mod` 3 /= x)

moveStr :: Point -> String -> Point
moveStr point "" = point
moveStr point s = moveStr (moveDir point dir) rest
    where (dir, rest) = if [head s] `elem` dirs then splitAt 1 s else splitAt 2 s

update :: State -> [Point] -> State
update = foldr ((runIdentity <$>) <$> Set.alterF (Identity . not))

simulate :: Int -> State -> State
simulate 0 !state = state
simulate t !state = simulate (t-1) $ nextState state

nextState :: State -> State
nextState points = update points changed
    where !changed = Set.toList $ foldl' findChanged Set.empty points
          neighbors = (`map` dirs) . moveDir
          numBlackNeighbors = length . filter (`Set.member` points) . neighbors
          getWhite = filter ((&&) <$> (`Set.notMember` points) <*> (==2) . numBlackNeighbors) . neighbors
          selfChange = (`notElem` [1,2]) . numBlackNeighbors
          getAll point = if selfChange point then point:(getWhite point) else getWhite point
          findChanged !partChanged = Set.union partChanged . Set.fromList . getAll