module Day16A where
    
import Data.Bits
import Data.Text (pack, splitOn, unpack)

type Reg = [Int]
type Ins = [Int]

data Note = Note Ins Reg Reg
    deriving Show

main :: IO ()
main = do
    text <- readFile "input.txt"
    let firstPart = unpack $ head $ splitOn (pack "\n\n\n") $ pack text
    let notes = map toNote $ chunks 4 $ lines $ firstPart
    let counts = map (\(Note i b a) -> length $ filter (\f -> f i b == a) functions) notes
    print $ length $ filter (>= 3) counts

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = let (a, b) = splitAt n xs in a : chunks n b

toNote :: [String] -> Note
toNote (before : instruction : after : _) = Note (map read $ words instruction) (read br) (read ar)
    where
        _ : br = dropWhile (/= ':') before
        _ : ar = dropWhile (/= ':') after

functions :: [Ins -> Reg -> Reg]
functions =
    [ addr
    , addi
    , mulr
    , muli
    , banr
    , bani
    , borr
    , bori
    , setr
    , seti
    , gtir
    , gtri
    , gtrr
    , eqir
    , eqri
    , eqrr
    ]

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
