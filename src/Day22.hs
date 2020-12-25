{-# LANGUAGE BangPatterns #-}

module Day22 (main) where

import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe

type Deck = [Int]
type State = (Deck, Deck)
type MetaState = (Set State, State)

main :: IO ()
main = do  
    contents <- readFile "input/22.txt"
    let [!deck1, !deck2] = map (map read . tail . lines) $ splitOn "\n\n" contents
    putStrLn $ show (solve1 (deck1, deck2)) ++ ", " ++ show (solve2 (deck1, deck2)) 

solve1 :: State -> Int
solve1 (deck, []) = score deck
solve1 ([], deck) = score deck
solve1 state = solve1 $ move1 state

move1 :: State -> State
move1 (deck1, deck2)
    | card1 > card2 = (rest1 ++ [card1, card2], rest2)
    | otherwise     = (rest1, rest2 ++ [card2, card1])
    where ((card1, rest1), (card2, rest2)) = (fromJust $ uncons deck1, fromJust $ uncons deck2)

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse

-- part 2 --

solve2 :: State -> Int
solve2 state = score $ snd $ playGame2 (Set.empty, state)

playGame2 :: MetaState -> (Bool, Deck)
playGame2 (_, (deck, [])) = (True, deck)
playGame2 (_, ([], deck)) = (False, deck)
playGame2 (!pastStates, state) 
    | Set.member state pastStates = (True, fst state)
    | otherwise                   = playGame2 (Set.insert state pastStates, move2 state)

move2 :: State -> State
move2 state@(deck1, deck2)
    | card1 > length rest1 || card2 > length rest2                   = move1 state
    | fst $ playGame2 (Set.empty, (take card1 rest1, take card2 rest2)) = (rest1 ++ [card1, card2], rest2)
    | otherwise                                                      = (rest1, rest2 ++ [card2, card1])
    where ((card1, rest1), (card2, rest2)) = (fromJust $ uncons deck1, fromJust $ uncons deck2)