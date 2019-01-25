import Data.Array as A (Array, (!), (//), listArray)
import Data.Array.IO
import Data.Array.MArray
import Data.Function
import Data.List (delete, minimumBy)
import Data.Map as M (Map, (!), (!?), empty, fromList, insert)

data Walker = Walker
    { position :: Point
    , status :: Status
    } deriving (Show, Eq)

type Point = (Int, Int)

data Status = Status
    { tool :: Tool
    , distance :: Int
    } deriving (Show, Eq)

data Tool = Torch | Gear | None
    deriving (Show, Eq, Ord)

depth, tx, ty :: Int
depth = 9171
tx = 7
ty = 721
cx = tx + 100
cy = ty + 100

main :: IO ()
main = do
    arr <- newArray ((0, 0), (cy, cx)) 0 :: IO (IOUArray (Int, Int) Int)
    erode 0 0 arr
    els <- getElems arr
    let cave = listArray ((0, 0), (cy, cx)) (map toChar els) // [((ty, tx), '.')]
    let dic = insert ((0, 0), Torch) 0 empty
    let result = shortest cave dic (ty, tx) (Walker (0, 0) $ Status Torch 0) []
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
shortest arr dic goal walker pending = if position walker == goal
    then walker
    else
        let
            newWalkers = out arr walker
            is = inserted dic newWalkers
        in
            if null is
                then
                    let
                        newMin = minimumBy (compare `on` dist) pending
                        newPending = delete newMin pending
                    in
                        shortest arr dic goal newMin newPending
                else
                    let
                        newDic = foldl (\dic' (Walker p (Status t d)) -> insert (p, t) d dic') dic is
                        all = is ++ pending
                        newMin = minimumBy (compare `on` dist) all
                        newPending = delete newMin all
                    in
                        shortest arr newDic goal newMin newPending

out :: Array Point Char -> Walker -> [Walker]
out arr (Walker position status) = map function $ zip ps cs
    where
        current = arr A.! position
        ps = points position
        cs = map (arr A.!) ps
        function (p, c) = Walker p $ reach status current c

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

reach :: Status -> Char -> Char -> Status
reach (Status tool distance) char1 char2 = if posible tool char2
    then Status tool (distance + 1)
    else Status (equipment M.! (char1, char2)) (distance + 8)

posible :: Tool -> Char -> Bool
posible tool chr = case tool of
    Torch -> chr /= '='
    Gear -> chr /= '|'
    None -> chr /= '.'

equipment :: Map (Char, Char) Tool
equipment = fromList
    [ (('.', '='), Gear)
    , (('.', '|'), Torch)
    , (('=', '.'), Gear)
    , (('=', '|'), None)
    , (('|', '.'), Torch)
    , (('|', '='), None)
    ]
