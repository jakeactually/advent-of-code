module Day3B where

data Rect = Rect
    { rectId :: Int
    , x :: Int
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
    let rects = map parseRect $ lines text
    print $ filter (\rect -> not $ any (overlaps rect) rects) rects

parseRect :: String -> Rect
parseRect str = Rect (int rectId) (int x) (int y) (int width) (int height)
    where
        (_ : rectId, _ : rect) = break (== '@') str
        (pos, _ : size) = break (== ':') rect
        (x, _ : y) = break (== ',') pos
        (width, _ : height) = break (== 'x') size

int :: String -> Int
int = read

overlaps :: Rect -> Rect -> Bool
overlaps r1 r2 = touchesBound r1 r2 && rectId r1 /= rectId r2

touchesBound :: Rect -> Rect -> Bool
touchesBound r1 r2 = not
     $ x r1 > x r2 + width r2
    || y r1 > y r2 + height r2
    || x r1 + width r1 < x r2
    || y r1 + height r1 < y r2
