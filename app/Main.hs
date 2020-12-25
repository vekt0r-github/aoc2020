module Main (main) where

import qualified System.Environment as Env
import qualified System.Exit as Exit

import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

main :: IO ()
main = Env.getArgs >>= parse >>= runDay

runDay :: [Int] -> IO ()
runDay [] = exit
runDay (n:ns) = action >> runDay ns
    where action = case n of 
            12 -> Day12.main
            13 -> Day13.main
            14 -> Day14.main
            15 -> Day15.main
            16 -> Day16.main
            17 -> Day17.main
            18 -> Day18.main
            19 -> Day19.main
            20 -> Day20.main
            21 -> Day21.main
            22 -> Day22.main
            23 -> Day23.main
            24 -> Day24.main
            25 -> Day25.main
            _  -> die

parse :: [String] -> IO [Int]
parse ["--all"] = return [12..25]
parse ["-v"]    = version >> exit
parse ["-h"]    = usage   >> exit
parse []        = usage   >> exit
parse ns        = return $ map read ns

usage   = do
    name <- Env.getProgName
    putStrLn $ "Usage: " ++ name ++ " -v | -h | --all | [<day_number> ..]"
version = putStrLn "Haskell aoc2020 0.1.0.0"
exit    = Exit.exitWith Exit.ExitSuccess
die     = Exit.exitWith (Exit.ExitFailure 1)