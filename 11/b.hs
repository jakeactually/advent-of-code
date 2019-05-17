{-# LANGUAGE TupleSections #-}

module Day11B where

import Control.Monad
import Data.Array.IO
import Data.Function
import Data.List
import Data.Maybe

size :: Int
size = 300 * 300 - 1

main :: IO ()
main = do
    arr <- newArray (0, size) 0 :: IO (IOUArray Int Int)    
    mapM_ (\i -> writeArray arr i $ toPower i) [0..size]
    areas <- mapM (\i -> mapM (\s -> (, (i, s)) <$> power arr i s) $ sizes i) [0..size]
    print $ maximumBy (compare `on` fst) $ concat areas

toPower :: Int -> Int
toPower i = power
    where
        (x', y') = toVec i
        (x, y) = (x' + 1, y' + 1)
        rackId = x + 10
        start = rackId * y
        processed = (start + 6392) * rackId
        hundreds = if processed > 99 then div (mod processed 1000) 100 else 0
        power = hundreds - 5

toVec :: Int -> (Int, Int)
toVec i = (mod i 300, div i 300)

toInt :: (Int, Int) -> Int
toInt (x, y) = y * 300 + x

grid :: Int -> Int -> [Int]
grid i size = let (x, y) = toVec i in do
    yo <- [0..size - 1]
    xo <- [0..size - 1]
    return $ toInt (x + xo, y + yo)

sizes :: Int -> [Int]
sizes i = [1..300 - (max x y)]
    where (x, y) = toVec i

power :: IOUArray Int Int -> Int -> Int -> IO Int
power arr i size = foldM (\acc j -> (acc +) <$> readArray arr j) 0 $ grid i size
