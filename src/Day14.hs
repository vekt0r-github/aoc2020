module Day14 (main) where

import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map)

type Line = (String, Int, Int)

main :: IO ()
main = do  
    contents <- readFile "input/14.txt"
    let dataList = reverse $ foldr (++) [] $ map parseBlock $ tail $ splitOn "mask = " contents
    putStrLn $ (show $ solve1 dataList) ++ ", " ++ (show $ solve2 dataList)

parseBlock :: String -> [Line]
parseBlock s = map (parseLine mask) $ tail $ lines s
    where mask = head $ lines s

parseLine :: String -> String -> Line
parseLine mask (_:_:_:_:s) = tuple $ map read $ splitOn "] = " s
    where tuple l = (mask, l!!0, l!!1)

binInv :: String -> Int
binInv "" = 0
binInv s = (2 * (binInv $ init s)) + (read $ [last s])

bin :: Int -> String
bin 0 = ""
bin n = (bin $ div n 2) ++ (show $ mod n 2)

pad :: String -> String
pad s
    | length s == 36 = s
    | otherwise      = pad ('0':s)

solve1 :: [Line] -> Int
solve1 bs = Map.foldr (+) 0 $ createDict1 bs

createDict1 :: [Line] -> Map Int Int
createDict1 [] = Map.empty
createDict1 (b:bs) = Map.insert k v $ createDict1 bs
    where (k, v) = findPair1 b

findPair1 :: Line -> (Int, Int)
findPair1 (mask, i, v) = (i, binInv $ applyMask mask $ pad $ bin v)

applyMask :: String -> String -> String
applyMask "" "" = ""
applyMask (mc:mcs) (c:cs) = case mc of 
    'X' -> c  : applyMask mcs cs
    _   -> mc : applyMask mcs cs

-- part 2 --

solve2 :: [Line] -> Int
solve2 bs = Map.foldr (+) 0 $ createDict2 bs

createDict2 :: [Line] -> Map String Int
createDict2 [] = Map.empty
createDict2 (b:bs) = insertAll ks v $ createDict2 bs
    where (ks, v) = findPair2 b
          insertAll [] v m = m
          insertAll (k:ks) v m = Map.insert k v $ insertAll ks v m

findPair2 :: Line -> ([String], Int)
findPair2 (mask, i, v) = (getAll mask $ pad $ bin i, v)

getAll :: String -> String -> [String]
getAll "" "" = [""]
getAll (mc:mcs) (c:cs) = case mc of 
    'X' -> prepend '0' some ++ prepend '1' some
    '0' -> prepend c some
    '1' -> prepend '1' some
    where some = getAll mcs cs
          prepend c l = map (c:) l