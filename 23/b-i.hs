module Day23BI where

-- Tried to compute the intersections,
-- they are too much, it didn't work.

import Data.List
import Data.Function
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import Debug.Trace

data Vec3 = Vec3 Int Int Int
    deriving Show

data Nano = Nano
    { center :: Vec3
    , radius :: Int
    } deriving Show

data Box = Box
    { position :: Vec3
    , size :: Vec3
    } deriving Show

data In = In
    { box :: Box
    , depth :: Int
    } deriving Show

main :: IO ()
main = do
    text <- readFile "input.txt"
    let is = map (toIn . parseNano) $ lines text
    print $ length $ pass 1 is 

toIn :: Nano -> In
toIn n = In (toBox n) 1

toBox :: Nano -> Box
toBox (Nano (Vec3 x y z) radius) = Box position' size'
    where
        position' = Vec3 (x - radius) (y - radius) (z - radius)
        diameter = radius * 2 + 1
        size' = Vec3 diameter diameter diameter

pass :: Int -> [In] -> [In]
pass iter is = if iter > 0
    then pass (iter - 1) $ mapMaybe (uncurry maybeIntersect) $ pairs is
    else is

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map ((,) x) xs ++ pairs xs
pairs [] = []

maybeIntersect :: In -> In -> Maybe In
maybeIntersect (In b1 d1) (In b2 d2) = if boxIntersect b1 b2
    then Just $ In (boxDiff b1 b2) (d1 + d2)
    else Nothing

boxIntersect :: Box -> Box -> Bool
boxIntersect (Box (Vec3 x1 y1 z1) (Vec3 w1 h1 d1)) (Box (Vec3 x2 y2 z2) (Vec3 w2 h2 d2)) = not
     $ x1 >= x2 + w2
    || x2 >= x1 + w1
    || y1 >= y2 + h2
    || y2 >= y1 + h1
    || z1 >= z2 + d2
    || z2 >= z1 + d1

boxDiff :: Box -> Box -> Box
boxDiff (Box (Vec3 x1 y1 z1) (Vec3 w1 h1 d1)) (Box (Vec3 x2 y2 z2) (Vec3 w2 h2 d2)) = Box position' size'
        where
            (maxX, minX, w) = if x1 > x2 then (x1, x2, w1) else (x2, x1, w2)
            (maxY, minY, h) = if y1 > y2 then (y1, y2, h1) else (y2, y1, h2)
            (maxZ, minZ, d) = if z1 > z2 then (z1, z2, d1) else (z2, z1, d2)
            position' = Vec3 maxX maxY maxZ
            size' = Vec3 (minX + w - maxX) (minY + h - maxY) (minZ + d - maxZ)

manhathan :: Vec3 -> Vec3 -> Int
manhathan (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    abs (x1 - x2) +
    abs (y1 - y2) +
    abs (z1 - z2)

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
