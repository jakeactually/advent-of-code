module Day23B where

-- Based on this amazing solution
-- https://www.reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/ecdqzdg?utm_source=share&utm_medium=web2x

import Data.List
import Text.Parsec

data Vec3 = Vec3 Int Int Int
    deriving Show

data Nano = Nano
    { pos :: Vec3
    , radius :: Int
    } deriving Show

main :: IO ()
main = do
    text <- readFile "input.txt"
    let nanos = map parseNano $ lines text
    let set = sortOn fst $ nanos >>= entries
    print $ best 0 0 0 set

entries :: Nano -> [(Int, Int)]
entries (Nano (Vec3 x y z) r) = [(d - r, 1), (d + r + 1, -1)]
    where
        d = abs x + abs y + abs z

best :: Int -> Int -> Int -> [(Int, Int)] -> Int
best count maxCount dist points = case points of
    (p : ps) -> if newCount > maxCount
        then loop newCount newCount newDist ps
        else loop newCount maxCount dist ps
        where
            (newDist, startOrEnd) = p
            newCount = count + startOrEnd
    [] -> dist

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
