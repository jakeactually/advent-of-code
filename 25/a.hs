module Day25A where

import Data.List
import Data.Text (pack, splitOn, unpack)

data Point = Point Int Int Int Int
    deriving Show

main :: IO  ()
main = do
    text <- readFile "input.txt"
    let points = map parsePoint $ lines text
    print $ length $ loop points []

parsePoint :: String -> Point
parsePoint str = Point x y z w
    where
        [x, y, z, w] = map (read . unpack) $ splitOn (pack ",") (pack str)

manhathan :: Point -> Point -> Int
manhathan (Point x1 y1 z1 w1) (Point x2 y2 z2 w2) =
    abs (x1 - x2) +
    abs (y1 - y2) +
    abs (z1 - z2) +
    abs (w1 - w2)

loop :: [Point] -> [[Point]] -> [[Point]]
loop (p : ps) constellations = if null near
    then loop ps $ [p] : constellations
    else loop ps $ (p : concat near) : far
    where
        (near, far) = partition (any $ (3 >=) . manhathan p) constellations
loop [] constellations = constellations
