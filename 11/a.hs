module Day11A where

import Data.Array.IO
import Data.Function
import Data.List

main :: IO ()
main = do
    arr <- newArray ((0, 0), (299, 299)) 0 :: IO (IOUArray (Int, Int) Int)
    fillArray arr
    powers <- mapM (coordsToPower arr) (grid 297 (0, 0))
    print $ maximumBy (compare `on` snd) powers

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
grid size (x, y) = do
    x' <- [x..x + size]
    y' <- [y..y + size]
    return (x', y')

coordsToPower :: IOUArray (Int, Int) Int -> (Int, Int) -> IO ((Int, Int), Int)
coordsToPower arr coords = do
    p <- power arr (grid 2 coords)
    return (coords, p)

power :: IOUArray (Int, Int) Int -> [(Int, Int)] -> IO Int
power arr idxs = sum <$> mapM (readArray arr) idxs
