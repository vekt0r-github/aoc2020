{-# LANGUAGE BangPatterns #-}

module Day25 (main) where

type State = (Int, Int)

main :: IO ()
main = do  
    contents <- readFile "input/25.txt"
    let [pubKey1, pubKey2] = map read $ lines contents
    let !privKey2 = snd $ transform (1, 0) 7 ((==pubKey2) . fst)
    print $ fst $ transform (1, 0) pubKey1 ((==privKey2) . snd)

p :: Int
p = 20201227

transform :: State -> Int -> (State -> Bool) -> State
transform state@(!value, !steps) subject stopMode
    | stopMode state = state
    | otherwise      = transform ((value * subject) `mod` p, steps+1) subject stopMode