module Day12 (main) where

type WaypointPos = (Float, Float)
type BoatPos = (Float, Float)
type BoatDir = Float
type StatePart1 = (BoatPos, BoatDir)
type StatePart2 = (BoatPos, WaypointPos)

main :: IO ()
main = do  
    contents <- readFile "input/12.txt"
    let moveData = map splitLine . lines $ contents
    let instructions1 = map getInstPart1 moveData
    let initState1 = ((0, 0), 0)
    let (finalPos1, _) = foldl (\a b -> b a) initState1 instructions1
    let instructions2 = map getInstPart2 moveData
    let initState2 = ((0, 0), (10, 1))
    let (finalPos2, _) = foldl (\a b -> b a) initState2 instructions2
    putStrLn $ (show $ round $ l1Norm finalPos1) ++ ", " ++ (show $ round $ l1Norm finalPos2)

splitLine :: String -> (Char, Int)
splitLine s = (head s, (read $ tail s)::Int)

l1Norm :: (Float, Float) -> Float
l1Norm (x, y) = abs x + abs y

getInstPart1 :: (Char, Int) -> StatePart1 -> StatePart1
getInstPart1 (c, ni) ((x, y), f)
    | c == 'F'    = ((x + n * cos f, y + n * sin f), f)
    | elem c "EW" = ((x + n, y), f)
    | elem c "NS" = ((x, y + n), f)
    | elem c "LR" = ((x, y), f + n * pi/180)
    where n = (if elem c "WSR" then -1 else 1) * fromIntegral ni

getInstPart2 :: (Char, Int) -> StatePart2 -> StatePart2
getInstPart2 ('F', ni) ((x, y), (dx, dy)) = ((x + n * dx, y + n * dy), (dx, dy))
    where n = fromIntegral ni
getInstPart2 (c, ni) (b, (x, y)) = (b, case c of
    c | elem c "EW" -> (x + n, y)
    c | elem c "NS" -> (x, y + n)
    c | elem c "LR" -> (x * cos a - y * sin a, x * sin a + y * cos a)
    )
    where n = (if elem c "WSR" then -1 else 1) * fromIntegral ni
          a = n * pi/180