module Day3A where

import Data.Array

data Rect = Rect
    { x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    } deriving Show

fabricWidth, fabricHeight :: Int
fabricWidth = 1000
fabricHeight = 1000

main :: IO ()
main = do
    text <- readFile "input.txt"
    let fabric = listArray (0, fabricWidth * fabricHeight - 1) $ repeat 0
    let newFabric = foldr fill fabric $ map parseRect $ lines text
    print $ length $ filter (> 1) $ elems newFabric

parseRect :: String -> Rect
parseRect str = Rect (int x) (int y) (int width) (int height)
    where
        (id, _ : rect) = break (== '@') str
        (pos, _ : size) = break (== ':') rect
        (x, _ : y) = break (== ',') pos
        (width, _ : height) = break (== 'x') size

int :: String -> Int
int = read

fill :: Rect -> Array Int Int -> Array Int Int
fill rect arr = accum (+) arr $ do
    x' <- [x rect..x rect + width rect - 1]
    y' <- [y rect..y rect + height rect - 1]
    return (x' + y' * fabricWidth, 1)
