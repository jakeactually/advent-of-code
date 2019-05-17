module Day18B where
    
import Data.Array
import Data.List

-- the pattern appears in the 800th minute
-- repeats each 28 minutes
-- solution is: pattern[(1000000000 - 800) % 28]

main :: IO ()
main = do
    text <- readFile "cache.txt" >>= \s -> length s `seq` return s
    let width = length $ head $ lines $ text
    let height = length $ lines $ text
    let arr = listArray ((0, 0), (height - 1, width)) text
    let ixs = indices arr
    let els = elems $ foldl (\arr _ -> arr // map (\i -> (i, acre arr i)) ixs) arr [1]
    writeFile "cache.txt" els
    let trees = length $ filter (== '|') els
    let lumberyards = length $ filter (== '#') els
    print [trees, lumberyards, trees * lumberyards]

acre :: Array (Int, Int) Char -> (Int, Int) -> Char
acre arr (y, x) = case arr ! (y, x) of
    '.' -> if (length $ filter (== '|') adjacent) >= 3 then '|' else '.'
    '|' -> if (length $ filter (== '#') adjacent) >= 3 then '#' else '|'
    '#' -> if any (== '#') adjacent && any (== '|') adjacent then '#' else '.'
    other -> other
    where
        (_, (height, width)) = bounds arr
        adjacent = map (arr !) $ filter (\(y, x) -> y >= 0 && x >= 0 && y <= height && x <= width)
            [ (y - 1, x - 1)
            , (y - 1, x)
            , (y - 1, x + 1)
            , (y, x + 1)
            , (y + 1, x + 1)
            , (y + 1, x)
            , (y + 1, x - 1)
            , (y, x - 1)
            ]
