module Day15B where

import Control.Monad
import Data.Array.IO
import Data.Function
import Data.List
import Data.Maybe
import Data.Set

type Point = (Int, Int)

data Cell
    = Wall
    | Space
    | Elve Int Int
    | Gnome Int Int
    | NewLine deriving (Show, Eq)

main :: IO ()
main = do
    text <- readFile "input.txt"
    let cells = Prelude.map toCell text
    arr <- newListArray ((0, 0), (31, 32)) cells :: IO (IOArray (Int, Int) Cell)
    rounds <- loop arr 0
    ps <- players arr
    cells <- mapM (readArray arr) ps
    let s = sum $ Prelude.map hp cells
    print [rounds, s, rounds * s]

loop :: IOArray (Int, Int) Cell -> Int -> IO Int
loop arr i = do
    ps <- players arr
    continue <- advance arr ps
    --render arr
    --print i
    if continue then loop arr (i + 1) else return i

toCell :: Char -> Cell
toCell chr = case chr of
    '.' -> Space
    'E' -> Elve 200 40
    'G' -> Gnome 200 3
    '#' -> Wall
    '\n' -> NewLine

toChar :: Cell -> Char
toChar chr = case chr of
    Space -> '.'
    Elve _ _ -> 'E'
    Gnome _ _ -> 'G'
    Wall -> '#'
    NewLine -> '\n'

render :: IOArray Point Cell -> IO ()
render arr = putStrLn . Prelude.map toChar =<< getElems arr

renderH :: IOArray Point Cell -> IO ()
renderH arr = mapM_ (print <=< readArray arr) =<< players arr

isWall :: Cell -> Bool
isWall Wall = True
isWall _ = False 

isSpace :: Cell -> Bool
isSpace Space = True
isSpace _ = False 

isPlayer :: Cell -> Bool
isPlayer (Elve _ _) = True
isPlayer (Gnome _ _) = True
isPlayer _ = False 

isGoal :: Cell -> Cell -> Bool
isGoal (Elve _ _) (Gnome _ _) = True
isGoal (Gnome _ _) (Elve _ _) = True
isGoal _ _ = False

isFoe :: Cell -> Cell -> Bool
isFoe (Elve _ _) (Gnome _ _) = True
isFoe (Gnome _ _) (Elve _ _) = True
isFoe (Elve _ _) (Elve _ _) = False
isFoe (Gnome _ _) (Gnome _ _) = False

hp :: Cell -> Int
hp (Gnome h _) = h
hp (Elve h _) = h

power :: Cell -> Int
power (Gnome _ a) = a
power (Elve _ a) = a

setHp :: Cell -> Int -> Cell
setHp (Gnome _ attack) nh = Gnome nh attack
setHp (Elve _ attack) nh = Elve nh attack

allDirs :: Point -> [Point]
allDirs (y, x) =
    [   (y - 1, x)
    ,   (y, x - 1)
    ,   (y, x + 1)
    ,   (y + 1, x)
    ]

players :: IOArray Point Cell -> IO [Point]
players arr = Prelude.map fst . Prelude.filter (isPlayer . snd) <$> getAssocs arr

out :: IOArray Point Cell -> Point -> IO [Point]
out arr (x, y) = filterM (fmap (not . isWall) . readArray arr) $ allDirs (x, y)

outs :: IOArray Point Cell -> (Set Point, [Point]) -> IO (Set Point, [Point])
outs arr (set, points) = do
    ps <- concat <$> mapM (out arr) points
    let filtered = fromList ps Data.Set.\\ set
    let newSet = Data.Set.union set filtered
    return (newSet, toList filtered)

shortest :: IOArray Point Cell -> (Set Point, [Point]) -> Cell -> Int -> IO (Maybe Int)
shortest arr (set, points) cell level = if Prelude.null points then return Nothing else do
    (s, ps) <- outs arr (set, points)
    cells <- mapM (readArray arr) ps
    if any (isGoal cell) cells then return (Just level) else
        let spaces = Prelude.map fst $ Prelude.filter (isSpace . snd) $ zip ps cells
        in shortest arr (s, spaces) cell (level + 1)

shortestW :: IOArray Point Cell -> Cell -> Point  -> IO (Maybe Int)
shortestW arr cell point = shortest arr (empty, [point]) cell 0

advance :: IOArray Point Cell -> [Point] -> IO Bool
advance arr (p : ps) = do
    cell <- readArray arr p
    if not $ isPlayer cell then advance arr ps else do
        movement <- move arr p
        killed <- case movement of
            Just np -> swap arr p np >> attack arr np
            Nothing -> attack arr p
        case killed of
            Just kp -> do
                -- check for foes
                ps' <- players arr
                foes <- filterM (fmap (isFoe cell) . readArray arr) ps'
                if not $ Prelude.null foes
                    then advance arr $ Data.List.delete kp ps
                    else return False  
            Nothing -> advance arr ps
advance _ _ = return True

move :: IOArray Point Cell -> Point -> IO (Maybe Point)
move arr player = do
    cell <- readArray arr player
    dirs <- out arr player
    near <- mapM (readArray arr) dirs
    if any (isGoal cell) near then return Nothing else do
        spaces <- filterM (fmap isSpace . readArray arr) dirs 
        distances <- mapM (shortestW arr cell) spaces
        let filtered = Prelude.filter (isJust . snd) (zip spaces distances)
        let sorted = sortBy (compare `on` fromJust . snd) filtered
        return $ fst . fst <$> uncons sorted

swap :: IOArray Point Cell -> Point -> Point -> IO ()
swap arr p1 p2 = do
    p <- readArray arr p1
    Space <- readArray arr p2
    writeArray arr p2 p
    writeArray arr p1 Space

attack :: IOArray Point Cell -> Point -> IO (Maybe Point)
attack arr player = do
    cell <- readArray arr player
    dirs <- out arr player
    near <- mapM (readArray arr) dirs
    let foes = Prelude.filter (isFoe cell . snd) $ Prelude.filter (isPlayer . snd) $ zip dirs near
    if Prelude.null foes then return Nothing else do
        let foe = fst $ minimumBy (compare `on` hp . snd) foes
        killed <- damage arr foe $ power cell
        return $ if killed then Just foe else Nothing 

damage :: IOArray Point Cell -> Point -> Int -> IO Bool
damage arr player amount = do
    p <- readArray arr player
    let nh = hp p - amount
    if nh > 0
        then writeArray arr player (setHp p nh) >> return False       
        else writeArray arr player Space >> return True
