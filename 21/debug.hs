import Data.Bits
import Data.Map as M (Map, (!), fromList)
import Data.Sequence as S (Seq, fromList, index)
import Debug.Trace

data Ins = Ins String [Int]
    deriving Show

type Reg = [Int]

main :: IO ()
main = do
    text <- readFile "input.txt"
    let ls = lines text
    let (binding : instructions) = ls
    let parsed = S.fromList $ map parseInstruction instructions
    let initial = [0, 0, 0, 0, 0, 0]
    print $ run (read $ last $ words binding) (0, initial) parsed

parseInstruction :: String -> Ins
parseInstruction str = Ins opcode $ map read rest
    where
        (opcode : rest) = words str

run :: Int -> (Int, Reg) -> Seq Ins -> (Int, Reg)
run binding (pointer, reg) ins = if pointer >= length ins then (pointer, reg) else
    if pointer == 29
        then trace (op ++ " " ++ show ns ++ " " ++ show after) $ run binding (newPointer, after) ins
        else run binding (newPointer, after) ins
        where
            Ins op ns = index ins pointer
            before = update binding pointer reg
            after = (functions ! op) ns before
            newPointer = after !! binding + 1

functions :: Map String ([Int] -> Reg -> Reg)
functions = M.fromList
    [ ("addr", addr)
    , ("addi", addi)
    , ("mulr", mulr)
    , ("muli", muli)
    , ("banr", banr)
    , ("bani", bani)
    , ("borr", borr)
    , ("bori", bori)
    , ("setr", setr)
    , ("seti", seti)
    , ("gtir", gtir)
    , ("gtri", gtri)
    , ("gtrr", gtrr)
    , ("eqir", eqir)
    , ("eqri", eqri)
    , ("eqrr", eqrr)
    ]    

update :: Int -> a -> [a] -> [a]
update i x xs = let (a, b : bs) = splitAt i xs in a ++ x : bs 

insr :: (Int -> Int -> Int) -> [Int] -> Reg -> Reg
insr function [a, b, c] reg = update c (function (reg !! a) (reg !! b)) reg

insi :: (Int -> Int -> Int) -> [Int] -> Reg -> Reg
insi function [a, b, c] reg = update c (function (reg !! a) b) reg

addr, addi, mulr, muli, banr, bani, borr, bori :: [Int] -> Reg -> Reg
addr = insr (+)
addi = insi (+)
mulr = insr (*)
muli = insi (*)
banr = insr (.&.)
bani = insi (.&.)
borr = insr (.|.)
bori = insi (.|.)

setr :: [Int] -> Reg -> Reg
setr [a, b, c] reg = update c (reg !! a) reg

seti :: [Int] -> Reg -> Reg
seti [a, b, c] reg = update c a reg

gtir :: [Int] -> Reg -> Reg
gtir [a, b, c] reg = update c (if a > reg !! b then 1 else 0) reg

gtri :: [Int] -> Reg -> Reg
gtri [a, b, c] reg = update c (if reg !! a > b then 1 else 0) reg

gtrr :: [Int] -> Reg -> Reg
gtrr [a, b, c] reg = update c (if reg !! a > reg !! b then 1 else 0) reg

eqir :: [Int] -> Reg -> Reg
eqir [a, b, c] reg = update c (if a == reg !! b then 1 else 0) reg

eqri :: [Int] -> Reg -> Reg
eqri [a, b, c] reg = update c (if reg !! a == b then 1 else 0) reg

eqrr :: [Int] -> Reg -> Reg
eqrr [a, b, c] reg = update c (if reg !! a == reg !! b then 1 else 0) reg
