module Day11B where

import Control.Monad
import Data.Array.IO
import Data.Function

data Record = Record 
    {   pos :: (Int, Int)
    ,   size :: Int
    ,   total :: Int
    } deriving (Show, Eq)

instance Ord Record where
    compare = compare `on` total

main :: IO ()
main = do
    arr <- newArray ((0, 0), (299, 299)) 0 :: IO (IOUArray (Int, Int) Int)
    fillArray arr
    --records <- mapM (zone arr) (grid 299 (0, 0))
    --print $ maximum $ concat records
    mapM_ (zone arr) (grid 299 (0, 0))

fillArray :: IOUArray (Int, Int) Int -> IO ()
fillArray arr = mapM_ (\coords -> writeArray arr coords (value coords)) (grid 299 (0, 0))

value :: (Int, Int) -> Int
value (x, y) = hundreds - 5
    where
        rackId = x + 10
        start = rackId * y
        processed = (start + 6392) * rackId
        hundreds = if processed > 99 then (processed `mod` 1000) `div` 100 else 0

grid :: Int -> (Int, Int) -> [(Int, Int)]
grid s (x, y) = do
    x' <- [x..x + s]
    y' <- [y..y + s]
    return (x', y')

layer :: IOUArray (Int, Int) Int -> Record -> IO Record
layer arr (Record (x, y) s currentSum) = do
    let coords = (x + s, y + s) : concat [ [(x + i, y + s), (x + s, y + i)] | i <- [0..s - 1] ]
    total <- sum <$> mapM (readArray arr) coords
    return $ Record (x, y) (s + 1) (currentSum + total)

zone :: IOUArray (Int, Int) Int -> (Int, Int) -> IO [Record]
zone arr p = do
    print p
    zs <- foldM (\(r : rs) _ -> (: r : rs) <$> layer arr r) [Record p 1 1] [1..limit]
    save zs
    return zs
    where
        (x, y) = p
        limit = 300 - max x y - 1

save :: [Record] -> IO ()
save = appendFile "log.txt" . unlines . map show
