import Data.List
import Data.Function
import Data.Map (Map, empty, insertWith, toList)

data Record = Record
    { minute :: Int
    , stamp :: Int
    , action :: Action
    } deriving Show

data Action = Guard Int | Sleep | Wake
    deriving (Show, Eq)

main :: IO ()
main = do
    text <- readFile "input.txt"
    let records = sortOn stamp $ map parse $ lines text
    let guardDatas = toList $ intervals 0 True records empty
    let maxMin = maximumBy (compare `on` length) . group . sort
    let gs = map (\(g, ms) -> let mms = maxMin ms in (length mms, (g, head mms))) guardDatas
    print $ maximumBy (compare `on` fst) gs

parse :: String -> Record
parse str = Record (int minute) intStamp action
    where
        (_ : stamp, _ : _ : description) = break (== ']') str
        (date, _ : time) = break (== ' ') stamp
        (month, _ : day) = break (== '-') $ drop 5 date
        (hour, _ : minute) = splitAt 2 time
        (prefix, _ : rest) = splitAt 5 description
        intStamp = int month * 44640 + int day * 1440 + int hour * 60 + int minute
        action
            | prefix == "Guard" = Guard $ int $ tail $ head $ words rest
            | prefix == "falls" = Sleep
            | prefix == "wakes" = Wake

int :: String -> Int
int = read

intervals :: Int -> Bool -> [Record] -> Map Int [Int] -> Map Int [Int]
intervals prevGuardId prevAwake records dic = case records of
    (r1 : r2 : rs) -> if not awake
        then insertWith (++) guardId minutes nextDic
        else nextDic
            where
                nextDic = intervals guardId awake (r2 : rs) dic
                guardId = case action r1 of
                    Guard newId -> newId
                    _ -> prevGuardId
                minutes = [minute r1 .. minute r2 - 1]
                awake = action r1 /= Sleep
    _ -> dic
