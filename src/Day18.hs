module Day18 (main) where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import Language.Haskell.Interpreter

type State = (Map Int Int, Int)

main :: IO ()
main = do
    contents <- readFile "input/18.txt"
    let equations = lines contents
    result1 <- calculate map1 equations 
    result2 <- calculate map2 equations 
    putStrLn $ show result1 ++ ", " ++ show result2

calculate :: (String -> String) -> [String] -> IO Int 
calculate func eqs = do
    let newEqs = map func eqs
    result <- runInterpreter $ setImports ["Prelude"] >> eval ("("++intercalate ")+(" newEqs++")")
    let res' = case result of Right x -> read x :: Int
    return res'

map1 :: String -> String 
map1 = reverse . revIter1 (Map.singleton 0 0, 0) . reverse

revIter1 :: State -> String -> String
revIter1 (p, pl) [] = replicate x '('
    where x = fromJust $ Map.lookup pl p
revIter1 state (c:s) = result ++ revIter1 newState s
    where (result, newState) = mapHelper state c

mapHelper :: State -> Char -> (String, State)
mapHelper (p, pl) c = case c of 
        ')' -> (")", (Map.insert (pl + 1) 1 p, pl + 1))
        '+' -> ("+)", (Map.insert pl (x + 1) p, pl))
        '*' -> ("*)", (Map.insert pl (x + 1) p, pl))
        '(' -> (replicate x '(', (p, pl - 1))
        c   -> ([c], (p, pl))
    where x = fromJust $ Map.lookup pl p

map2 :: String -> String
map2 = replace '*' ")*(" . replace ')' "))" . replace '(' "(("
    where replace c s = intercalate s . splitOn [c]