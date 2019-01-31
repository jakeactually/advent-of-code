import Data.List
import Data.Function
import Text.Parsec
import Text.Parsec.Char

data Pos = Pos
    { x :: Int
    , y :: Int
    , z :: Int
    } deriving Show

data Nano = Nano
    { pos :: Pos
    , radius :: Int
    } deriving Show

main :: IO ()
main = do
    text <- readFile "input.txt"
    let nanos = map parseNano $ lines text
    let largest = maximumBy (compare `on` radius) nanos
    let distances = map (manhathan (pos largest) . pos) nanos
    print $ length $ filter (<= radius largest) distances

parseNano :: String -> Nano
parseNano str = let Right nano = parse nanoParser "" str in nano

nanoParser :: Parsec String () Nano
nanoParser = do
    string "pos=<"
    [x, y, z] <- numberParser `sepBy` (char ',')
    string ">, r="
    r <- numberParser
    return $ Nano (Pos x y z) r

numberParser :: Parsec String () Int
numberParser = read <$> many (digit <|> char '-')

manhathan :: Pos -> Pos -> Int
manhathan (Pos x1 y1 z1) (Pos x2 y2 z2) =
    abs (x1 - x2) +
    abs (y1 - y2) +
    abs (z1 - z2)
