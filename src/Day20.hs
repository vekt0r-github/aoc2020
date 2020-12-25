{-# LANGUAGE BangPatterns, TupleSections #-}

module Day20 (main) where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe

type SquareCatalog = Map Int Square
type Square = Map (Int, Int) Bool
type NeighborCatalog = Map (Int, String) (Maybe (Int, String))
type Image = Map (Int, Int) (Int, String)

main :: IO ()
main = do  
    contents <- readFile "input/20.txt"
    let !squares = foldl' parseSquare Map.empty $ splitOn "\n\n" contents
    let !neighbors = makeNeighbors squares
    let !corners = getCorners squares neighbors
    let !image = makeImage neighbors $ head corners
    let !imageSquare = showImage squares image
    let !monsterLocs = makeMonster
            ["                  # ",
             "#    ##    ##    ###",
             " #  #  #  #  #  #   "]
    let !numMons = head $ filter (> 0) $ map (countMonsters monsterLocs imageSquare) allTransforms
    let !result = sum (Map.elems $ Map.map fromEnum imageSquare) - numMons * length monsterLocs
    putStrLn $ show (product corners) ++ ", " ++ show result

parseSquare :: SquareCatalog -> String -> SquareCatalog
parseSquare partSquares sqStr = Map.insert index square partSquares
    where (keyLine:rest) = lines sqStr
          !index = read $ init $ drop 5 keyLine
          zipLine (i, line) = zipWith (\j c -> ((i, j), c == '#')) [0..] line
          !square = Map.fromList $ concatMap zipLine $ zip [0..] rest

sqSize :: Int
sqSize = 10

imSize :: Int
imSize = 12

imSqSize :: Int
imSqSize = imSize * (sqSize-2)

allTransforms :: [String]
allTransforms = [replicate s 's' ++ replicate r 'r' | s <- [0,1], r <- [0..3]]

topRow :: Int -> ((Int, Int) -> Bool) -> [Bool]
topRow n f = map (curry f 0) [0..n-1]
          
applyFlip :: Int -> ((Int, Int) -> Bool) -> (Int, Int) -> Bool
applyFlip n f (a, b) = f (n-1-a, b)

applyRotate :: Int -> ((Int, Int) -> Bool) -> (Int, Int) -> Bool
applyRotate n f (a, b) = f (b, n-1-a)

applyTransform :: Int -> [Char] -> ((Int, Int) -> Bool) -> (Int, Int) -> Bool
applyTransform _ [] = id
applyTransform n (t:ts) = transform . applyTransform n ts
    where transform = case t of
              'r' -> applyRotate n
              's' -> applyFlip n

reduce :: String -> String
reduce s
    | s `elem` allTransforms = s
    | "rrrr" `isInfixOf` s   = reduce $ replace "rrrr" "" s
    | "ss" `isInfixOf` s     = reduce $ replace "ss" "" s
    | "rs" `isInfixOf` s    = reduce $ replace "rs" "srrr" s
    where replace d e = intercalate e . splitOn d

makeNeighbors :: SquareCatalog -> NeighborCatalog
makeNeighbors squares = Map.fromList $ map getPair allKeys
    where allKeys = (,) <$> Map.keys squares <*> allTransforms
          topRowTransform (x, s) = topRow sqSize $ applyTransform sqSize s ((squares Map.! x) Map.!)
          !borders = Map.fromListWith (++) $ map (\p -> (topRowTransform p, [p])) allKeys
          getPair p = (p, v)
              where results = borders Map.! topRowTransform p
                    v = case uncons $ filter (/= p) results of
                        Just ((x', s'), _) -> Just (x', reduce $ "s" ++ s')
                        Nothing            -> Nothing

neighborDirs :: NeighborCatalog -> Int -> [String]
neighborDirs neighbors x = filter (isJust . (neighbors Map.!) . (x,)) allTransforms

getCorners :: SquareCatalog -> NeighborCatalog -> [Int]
getCorners squares neighbors = filter ((<= 4) . length . neighborDirs neighbors) $ Map.keys squares

-- part 2 --

makeImage :: NeighborCatalog -> Int -> Image
makeImage neighbors corner = foldl' (findFit neighbors) (Map.singleton (0, 0) cornerUp) allPoints 
    where allPoints = tail $ (,) <$> [0..imSize-1] <*> [0..imSize-1]
          [or1, or2] = map length $ filter (notElem 's') $ neighborDirs neighbors corner
          orientation = reduce $ replicate ((abs (or1^2 - or2^2) * 3 + 3) `mod` 5 + 2) 'r' 
          cornerUp = (corner, orientation)

findFit :: NeighborCatalog -> Image -> (Int, Int) -> Image
findFit neighbors partImage (a, b) = Map.insert (a, b) newTile partImage
    where ((x, s), rot, rotInv) = case b of
              0 -> (partImage Map.! (a-1, b), "rr", "rr")
              _ -> (partImage Map.! (a, b-1), "r", "rrr")
          (x', s') = fromJust $ neighbors Map.! (x, reduce (rot ++ s))
          newTile = (x', reduce (rotInv ++ s'))

showImage :: SquareCatalog -> Image -> Square
showImage squares image = Map.fromList $ map getPair allKeys
    where allKeys = (,) <$> [0..imSqSize-1] <*> [0..imSqSize-1]
          getPair (a0, b0) = ((a0, b0), cell)
              where ((a, a'), (b, b')) = (a0 `divMod` (sqSize-2), b0 `divMod` (sqSize-2))
                    (x, s) = image Map.! (a, b)
                    cell = applyTransform sqSize s ((squares Map.! x) Map.!) (a'+1, b'+1)
              
countMonsters :: [(Int, Int)] -> Square -> String -> Int
countMonsters monsterLocs imageSquare s = length $ filter monster $ Map.keys imageSquare
    where !monSize = let (as, bs) = unzip monsterLocs in (maximum as, maximum bs)
          monster (a, b) = not (Map.notMember (add monSize) imageSquare) &&
              all (applyTransform imSqSize s (imageSquare Map.!) . add) monsterLocs
              where add t = (a + fst t, b + snd t)

makeMonster :: [[Char]] -> [(Int, Int)]
makeMonster monsterStrs = concatMap zipLine $ zip [0..] monsterStrs
    where zipLine (i, line) = [(i, j) | (j, c) <- zip [0..] line, c == '#']