import Data.Array.IO
import Data.Array.MArray

main :: IO ()
main = do
    text <- readFile "cache.txt"
    let width = length $ head $ lines text
    let height = length $ lines text
    let distances = map (\c -> if any (== c) ".|-" then 0 else -1) text
    arr <- newListArray ((0, 0), (height - 1, width)) distances :: IO (IOUArray (Int, Int) Int)
    print =<< shortest [(99, 99)] 1 arr -- this should be substracted one and divided by two

shortest :: [(Int, Int)] -> Int -> IOUArray (Int, Int) Int -> IO Int
shortest points mark arr = do
    mapM_ (\p -> out p mark arr) points
    as <- getAssocs arr
    let next = map fst $ filter ((== mark) . snd) as
    if null next then return mark else shortest next (mark + 1) arr

out :: (Int, Int) -> Int -> IOUArray (Int, Int) Int -> IO ()
out (y, x) mark arr = flip mapM_ [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)] $ \p -> do
    cell <- readArray arr p
    if cell == 0
        then writeArray arr p mark
        else return ()
