module Day20B where
    
import Data.Array.IO
import Data.Array.MArray

main :: IO ()
main = do
    text <- readFile "cache.txt"
    let width = length $ head $ lines text
    let height = length $ lines text
    let distances = map (\c -> if any (== c) ".|-" then 0 else -1) text
    arr <- newListArray ((0, 0), (height - 1, width)) distances :: IO (IOUArray (Int, Int) Int)
    print =<< shortest 0 [(99, 99)] 1 arr

shortest :: Int -> [(Int, Int)] -> Int -> IOUArray (Int, Int) Int -> IO Int
shortest acc points mark arr = do
    mapM_ (\p -> out p mark arr) points
    as <- getAssocs arr
    let next = map fst $ filter ((== mark) . snd) as
    if null next
    then return acc
    else if mark >= 1999 && mod mark 2 == 1
    then shortest (acc + length next) next (mark + 1) arr
    else shortest acc next (mark + 1) arr

out :: (Int, Int) -> Int -> IOUArray (Int, Int) Int -> IO ()
out (y, x) mark arr = flip mapM_ [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)] $ \p -> do
    cell <- readArray arr p
    if cell == 0
        then writeArray arr p mark
        else return ()
