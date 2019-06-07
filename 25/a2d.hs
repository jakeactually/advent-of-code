module Day25A2d where

import Data.List

data Point = Point Int Int
    deriving Show

main :: IO  ()
main = do
    text <- readFile "input2d.txt"
    let points = map parsePoint $ lines text
    print $ length $ loop points []

parsePoint :: String -> Point
parsePoint str = Point (read x) (read y)
    where
        (x, _ : y) = break (== ',') str

manhathan :: Point -> Point -> Int
manhathan (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

loop :: [Point] -> [[Point]] -> [[Point]]
loop (p : ps) constellations = if null near
    then loop ps $ [p] : constellations
    else loop ps $ (p : concat near) : far
    where
        (near, far) = partition (any $ (3 >=) . manhathan p) constellations
loop [] constellations = constellations
