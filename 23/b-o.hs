module Day23BO where

-- Octree approach
-- couldn't get it to work

import Data.List
import Data.Ord
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.Char

data Vec3 = Vec3 Int Int Int
    deriving (Show, Eq)

data Nano = Nano
    { pos :: Vec3
    , radius :: Int
    } deriving Show

data Cube = Cube
    { cpos :: Vec3
    , size :: Int
    } deriving (Show, Eq)

data Scan = Scan Cube Int
    deriving (Show, Eq)

main :: IO ()
main = do
    text <- readFile "input.txt"
    let nanos = map parseNano $ lines text
    let start = Vec3 (-2 ^ 27) (-2 ^ 27) (-2 ^ 27)
    let cube = Cube start (2 ^ 28)
    passes nanos S.empty $ Scan cube 1000

passes :: [Nano] -> S.Set Scan -> Scan -> IO ()
passes nanos queue (Scan cube _) = if size cube == 1 
    then return ()
    else
    print mx >>
    passes nanos newQueue mx
    where
    result = map (`reach` nanos) $ octree cube
    merged = foldr S.insert queue result
    mx = maximum merged
    newQueue = S.delete mx merged

instance Ord Scan where
    compare (Scan (Cube p1 s1) i1) (Scan (Cube p2 s2) i2) =
        if i1 > i2 then GT
        else if i1 == i2 then
            if s1 < s2 then GT
            else if s1 == s2 then
            if origin p1 < origin p2 then GT
            else if origin p1 == origin p2 then EQ
            else LT
            else LT
        else LT

origin :: Vec3 -> Int
origin = manhathan (Vec3 0 0 0)

octree :: Cube -> [Cube]
octree (Cube (Vec3 x y z) size) = map (`Cube` s) $ quadrants
    where
    s = div size 2
    quadrants =
        [ Vec3 x y z
        , Vec3 (x + s) y z
        , Vec3 x (y + s) z
        , Vec3 x y (z + s)
        , Vec3 (x + s) (y + s) z
        , Vec3 x (y + s) (z + s)
        , Vec3 (x + s) y (z + s)
        , Vec3 (x + s) (y + s) (z + s)
        ]

reach :: Cube -> [Nano] -> Scan
reach cube nanos = Scan cube len
    where
    len = length $ filter (`touches` cube) nanos

touches :: Nano -> Cube -> Bool
touches nano cube = m <= radius
    where
    Nano (Vec3 x y z) radius = nano
    Cube (Vec3 cx cy cz) size = cube
    dx = clamp x cy (cx + size)
    dy = clamp y cy (cy + size)
    dz = clamp z cz (cz + size)
    m = manhathan (Vec3 dx dy dz) (Vec3 x y z)

clamp :: Int -> Int -> Int -> Int
clamp arg lo hi = min (max arg lo) (hi - 1)

manhathan :: Vec3 -> Vec3 -> Int
manhathan (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    abs (x1 - x2) +
    abs (y1 - y2) +
    abs (z1 - z2)

-- Parse

parseNano :: String -> Nano
parseNano str = let Right nano = parse nanoParser "" str in nano

nanoParser :: Parsec String () Nano
nanoParser = do
    string "pos=<"
    [x, y, z] <- numberParser `sepBy` (char ',')
    string ">, r="
    r <- numberParser
    return $ Nano (Vec3 x y z) r

numberParser :: Parsec String () Int
numberParser = read <$> many (digit <|> char '-')
    