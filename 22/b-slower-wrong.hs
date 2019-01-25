import Data.Array
import Data.Array.IO
import Data.Array.MArray
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map as M (Map, (!?), empty, insert)

data Walker = Walker
    { position :: Point
    , status :: Status
    } deriving Show

type Point = (Int, Int)

data Status = Status
    { tool :: Tool
    , distance :: Int
    } deriving Show

data Tool = Torch | Gear | None
    deriving (Show, Eq, Ord)

depth, tx, ty :: Int
depth = 510
tx = 10
ty = 10
cx = 20
cy = 20

main :: IO ()
main = do
    arr <- newArray ((0, 0), (cy, cx)) 0 :: IO (IOUArray (Int, Int) Int)
    erode 0 0 arr
    els <- getElems arr
    let cave = listArray ((0, 0), (cy, cx)) (map toChar els) // [((ty, tx), '.')]
    let result = shortest cave M.empty (ty, tx) (Walker (0, 0) $ Status Torch 999999) [Walker (0, 0) $ Status Torch 0]
    print result

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = let (a, b) = splitAt n xs in a : chunks n b

dist :: Walker -> Int
dist = distance . status

erode :: Int -> Int -> IOUArray (Int, Int) Int -> IO ()
erode y x arr = do
    gi <- getGi y x arr
    let erosion = mod (gi + depth) 20183
    writeArray arr (y, x) erosion
    if y == cy && x == cx then return ()
    else if x == cx then erode (y + 1) 0 arr
    else erode y (x + 1) arr

getGi :: Int -> Int -> IOUArray (Int, Int) Int -> IO Int
getGi y x arr =
    if y == 0 && x == 0 then return 0
    else if y == 0 then return $ x * 16807
    else if x == 0 then return $ y * 48271
    else (*) <$> readArray arr (y, x - 1) <*> readArray arr (y - 1, x)

toChar :: Int -> Char
toChar erosion = case mod erosion 3 of
    0 -> '.'
    1 -> '='
    2 -> '|'

shortest :: Array Point Char -> Map (Point, Tool) Int -> Point -> Walker -> [Walker] -> Walker
shortest arr dic goal closest walkers =
    let
        newWalkers = map (out arr) walkers
        is = inserted dic $ concat newWalkers
        closer = filter (\i -> dist i <= dist closest) is
    in
        if null closer
            then closest
            else
                let
                    newDic = foldl (\dic' (Walker p (Status t d)) -> insert (p, t) d dic') dic closer
                    reached = filter ((==) goal . position) closer
                    newMin = minimumBy (compare `on` dist) $ closest : reached
                in
                    shortest arr newDic goal newMin closer

out :: Array Point Char -> Walker -> [Walker]
out arr (Walker position status) = zip ps cs >>= \(p, c) -> map (Walker p) (reach status c)
    where
        ps = points position
        cs = map (arr !) ps

inserted :: Map (Point, Tool) Int -> [Walker] -> [Walker]
inserted dic ((Walker p (Status t d)) : ws) = case dic !? (p, t) of
    Just lastDistance | d >= lastDistance -> inserted dic ws
    _ -> (Walker p (Status t d)) : inserted (insert (p, t) d dic) ws
inserted _ [] = []

points :: Point -> [Point]
points (y, x) = filter inBound
    [ (y - 1, x)
    , (y, x + 1)
    , (y + 1, x)
    , (y, x - 1)
    ]

inBound :: Point -> Bool
inBound (y, x)
     = y >= 0
    && x >= 0
    && y <= cy
    && x <= cx

reach :: Status -> Char -> [Status]
reach (Status tool distance) char = if posible tool char
    then [Status tool (distance + 1)]
    else map (flip Status (distance + 8)) $ available char

posible :: Tool -> Char -> Bool
posible tool chr = case tool of
    Torch ->  chr /= '='
    Gear -> chr /= '|'
    None -> chr /= '.'

available :: Char -> [Tool]
available char = case char of
    '.' -> [Torch, Gear]
    '=' -> [Gear, None]
    '|' -> [Torch, None]
