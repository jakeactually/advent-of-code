{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Data.Array as A
import Data.Map as M (Map, (!?), assocs, fromList, insert, delete)

type Pos = (Int, Int)
type Wagon = (Char, Dir)
type Cart = (Pos, Wagon)

data Dir = LDir | Straight | RDir
    deriving Show

main :: IO ()
main = do
    input <- readFile "input.txt"
    --print $ length $ lines input
    --print $ length $ head $ lines input
    let world = listArray ((0, 0), (149, 150)) input
    let posAndWagons = filter ((`elem` "><v^") . snd) $ A.assocs world
    let tracks = world // map (second toTrack) posAndWagons
    let carts = map (second (, LDir)) posAndWagons
    let state = fromList carts
    print $ tick tracks state carts

tick :: Array Pos Char -> Map Pos Wagon -> [Cart] -> Pos
tick tracks state (wagon@((y, x), (c, d)) : wagons) =
    let
        newPos = move y x c
    in
        case state !? newPos of
            Just _ ->
                newPos
            Nothing ->
                let
                    track = tracks ! newPos
                    newChar = if track == '+' then decide d c else turn track c
                    newDir = if track == '+' then dirLoop d else d
                    newWagon = (newChar, newDir)
                    newState = insert newPos newWagon $ delete (y, x) state
                in
                    tick tracks newState wagons
tick tracks state [] = tick tracks state $ M.assocs state

toTrack :: Char -> Char
toTrack c = case c of
    '>' -> '-'
    '<' -> '-'
    'v' -> '|'
    '^' -> '|'

turn :: Char -> Char -> Char
turn trackChar c =
    if trackChar == '/'
    then case c of
        '>' -> '^'
        '<' -> 'v'
        'v' -> '<'
        '^' -> '>'
    else if trackChar == '\\'
    then case c of
        '>' -> 'v'
        '<' -> '^'
        'v' -> '>'
        '^' -> '<'
    else c

move :: Int -> Int -> Char -> (Int, Int)
move y x c = case c of
    '>' -> (y, x + 1) 
    '<' -> (y, x - 1) 
    'v' -> (y + 1, x)
    '^' -> (y - 1, x) 

dirLoop :: Dir -> Dir
dirLoop dir = case dir of
    LDir -> Straight
    Straight -> RDir
    RDir -> LDir

decide :: Dir -> Char -> Char
decide dir char = case dir of
    LDir -> case char of
        '>' -> '^'
        '<' -> 'v'
        'v' -> '>'
        '^' -> '<'
    RDir -> case char of
        '>' -> 'v'
        '<' -> '^'
        'v' -> '<'
        '^' -> '>'
    Straight -> char
