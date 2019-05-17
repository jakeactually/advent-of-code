module Day22A where

import Data.Array.MArray
import Data.Array.IO

depth, tx, ty :: Int
depth = 9171
tx = 7
ty = 721

main :: IO ()
main = do
    arr <- newArray ((0, 0), (ty, tx)) 0 :: IO (IOUArray (Int, Int) Int)
    erode 0 0 arr
    as <- getAssocs arr
    target <- readArray arr (ty, tx)
    let s = sum $ map (flip mod 3 . snd) $ as
    print $ s - mod target 3

erode :: Int -> Int -> IOUArray (Int, Int) Int -> IO ()
erode y x arr = do
    gi <- getGi y x arr
    let erosion = mod (gi + depth) 20183
    writeArray arr (y, x) erosion
    if y == ty && x == tx then return ()
    else if x == tx then erode (y + 1) 0 arr
    else erode y (x + 1) arr

getGi :: Int -> Int -> IOUArray (Int, Int) Int -> IO Int
getGi y x arr =
    if y == 0 && x == 0 then return 0
    else if y == 0 then return $ x * 16807
    else if x == 0 then return $ y * 48271
    else (*) <$> readArray arr (y, x - 1) <*> readArray arr (y - 1, x)

render :: (Int, Int) -> Int -> Char
render (y, x) erosion = if x == tx
    then '\n'
    else case mod erosion 3 of
        0 -> '.'
        1 -> '='
        2 -> '|'
