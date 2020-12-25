module Day16 (main, findPerm) where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe

type Rule = (String, Int -> Bool)
type Options = String -> Int -> Bool
type Ticket = [Int]

main :: IO ()
main = do  
    contents <- readFile "input/16.txt"
    let [ruleStr, myStr, nearStr] = splitOn "\n\n" contents
    let rules = map parseRule $ lines ruleStr
    let myTicket = parseTicket $ lines myStr !! 1
    let nearTickets = map parseTicket $ tail $ lines nearStr
    putStrLn $ show (solve1 rules nearTickets) ++ ", " ++ show (solve2 rules nearTickets myTicket)

parseRule :: String -> Rule
parseRule s = (name, inRanges)
    where parseRange = map read . splitOn "-" :: String -> [Int]
          [name, rangesStr] = splitOn ": " s
          ranges = map parseRange $ splitOn " or " rangesStr
          inRange n [low, high] = n >= low && n <= high
          inRanges n = any (inRange n) ranges

parseTicket :: String -> Ticket
parseTicket s = map read $ splitOn "," s

solve1 :: [Rule] -> [Ticket] -> Int
solve1 rules nearTickets = sum $ filter (not . validInput rules) $ concat nearTickets

validInput :: [Rule] -> Int -> Bool
validInput rules n = any (satisfies n) rules
    where satisfies n r = snd r n

-- part 2 --

solve2 :: [Rule] -> [Ticket] -> Ticket -> Int
solve2 rules nearTickets' myTicket = product $ map ((myTicket!!) . permFunc) $ take 6 (map fst rules)
    where options = makeAllOptions rules nearTickets
          nearTickets = filter (all $ validInput rules) nearTickets'
          perm = findPerm options (map fst rules) $ take (length rules) [0..]
          permFunc = fromJust . flip Map.lookup perm

findPerm :: (Ord a, Eq b) => (a -> b -> Bool) -> [a] -> [b] -> Map a b
findPerm _ [] _ = Map.empty
findPerm _ _ [] = Map.empty
findPerm options as bs = case pair of 
    Just (a, b)  -> Map.insert a b $ findPerm options (delete a as) (delete b bs)
    Nothing -> Map.empty
    where pair = case findDetermined (flip options) bs as of
              Just (b, a) -> Just (a, b)
              Nothing -> findDetermined options as bs

findDetermined :: (a -> b -> Bool) -> [a] -> [b] -> Maybe (a, b)
findDetermined f as bs = case find ((==1) . length . options) as of
    Just detA -> Just (detA, head $ options detA)
    Nothing -> Nothing
    where options a = filter (f a) bs

makeAllOptions :: [Rule] -> [Ticket] -> Options
makeAllOptions rules tickets name index = all (ruleset name . (!!index)) tickets
    where ruleset = makeRuleset rules

makeRuleset :: [Rule] -> String -> Int -> Bool
makeRuleset (r:rs) s
    | s == fst r = snd r
    | otherwise  = makeRuleset rs s
