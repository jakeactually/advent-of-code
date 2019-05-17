module Day16B where
    
import Data.Bits
import Data.Text (pack, splitOn, unpack)

type Reg = [Int]
type Ins = [Int]

data Note = Note Ins Reg Reg
    deriving Show

main :: IO ()
main = do
    text <- readFile "input.txt"
    let secondPart = unpack $ (splitOn (pack "\n\n\n") $ pack text) !! 1
    let parsed = filter (not . null) $ map (map read . words) $ lines $ secondPart
    print $ foldl (\reg ins -> (ordered !! head ins) ins reg) [0, 0, 0, 0] $ parsed

toNote :: [String] -> Note
toNote (before : instruction : after : _) = Note (map read $ words instruction) (read br) (read ar)
    where
        _ : br = dropWhile (/= ':') before
        _ : ar = dropWhile (/= ':') after

functions :: [(String, Ins -> Reg -> Reg)]
functions =
    --("addr", addr) : -- 10
    --("addi", addi) : -- 15
    --("mulr", mulr) : -- 11
    --("muli", muli) : -- 13
    --("banr", banr) : -- 14
    --("bani", bani) : -- 12
    --("borr", borr) : -- 9
    --("bori", bori) : -- 2
    --("setr", setr) : -- 1
    --("seti", seti) : -- 6
    --("gtir", gtir) : -- 0
    --("gtri", gtri) : -- 4
    --("gtrr", gtrr) : -- 3
    --("eqir", eqir) : -- 5
    --("eqri", eqri) : -- 7
    --("eqrr", eqrr) : -- 8
    []

ordered :: [Ins -> Reg -> Reg]
ordered =
    gtir : -- 0
    setr : -- 1
    bori : -- 2
    gtrr : -- 3
    gtri : -- 4
    eqir : -- 5
    seti : -- 6
    eqri : -- 7
    eqrr : -- 8
    borr : -- 9
    addr : -- 10
    mulr : -- 11
    bani : -- 12
    muli : -- 13
    banr : -- 14
    addi : -- 15
    []

possible :: Note -> [String]
possible (Note instruction before after) =
    map fst $ filter (\function -> (snd function) instruction before == after) functions

update :: Int -> a -> [a] -> [a]
update i x xs = let (a, b : bs) = splitAt i xs in a ++ x : bs 

insr :: (Int -> Int -> Int) -> Ins -> Reg -> Reg
insr function [o, a, b, c] reg = update c (function (reg !! a) (reg !! b)) reg

insi :: (Int -> Int -> Int) -> Ins -> Reg -> Reg
insi function [o, a, b, c] reg = update c (function (reg !! a) b) reg

addr, addi, mulr, muli, banr, bani, borr, bori :: Ins -> Reg -> Reg
addr = insr (+)
addi = insi (+)
mulr = insr (*)
muli = insi (*)
banr = insr (.&.)
bani = insi (.&.)
borr = insr (.|.)
bori = insi (.|.)

setr :: Ins -> Reg -> Reg
setr [o, a, b, c] reg = update c (reg !! a) reg

seti :: Ins -> Reg -> Reg
seti [o, a, b, c] reg = update c a reg

gtir :: Ins -> Reg -> Reg
gtir [o, a, b, c] reg = update c (if a > reg !! b then 1 else 0) reg

gtri :: Ins -> Reg -> Reg
gtri [o, a, b, c] reg = update c (if reg !! a > b then 1 else 0) reg

gtrr :: Ins -> Reg -> Reg
gtrr [o, a, b, c] reg = update c (if reg !! a > reg !! b then 1 else 0) reg

eqir :: Ins -> Reg -> Reg
eqir [o, a, b, c] reg = update c (if a == reg !! b then 1 else 0) reg

eqri :: Ins -> Reg -> Reg
eqri [o, a, b, c] reg = update c (if reg !! a == b then 1 else 0) reg

eqrr :: Ins -> Reg -> Reg
eqrr [o, a, b, c] reg = update c (if reg !! a == reg !! b then 1 else 0) reg
