module Day13 (main) where

import Data.String
import Data.List

main :: IO ()
main = do  
    contents <- readFile "input/13.txt"
    let (first, datas) = parseData $ split (`elem` ",\n") contents
    let timeWait (x, _) = mod (-first) x
    let (bestTime, bestIndex) = minimum [(timeWait p, fst p) | p <- datas]
    let (_, result) = solveCRT datas (1, 0)
    putStrLn $ (show $ bestTime*bestIndex) ++ ", " ++ (show result)

split :: (Char -> Bool) -> String -> [String]
split p s = case break p s of
    (a, c:b) -> a : split p b
    (a, "")  -> [a]

parseData :: [String] -> (Int, [(Int, Int)])
parseData (f:xs) = (read f, [(read x, i) | (x, i) <- zip xs [0..length xs],  x /= "x"])

solveCRT :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
solveCRT [] ms = ms
solveCRT ((x, i):ps) (modulus, value)
    | mod value x == mod (-i) x = solveCRT ps (modulus*x, value)
    | otherwise                 = solveCRT ((x, i):ps) (modulus, value + modulus)

