import Data.Set

main :: IO ()
main = do
    text <- readFile "input.txt"
    print $ dupFreq 0 empty $ cycle $ Prelude.map parse $ lines text

dupFreq :: Int -> Set Int -> [Int] -> Int
dupFreq acc record (x : xs) = if member res record then res else dupFreq res (insert res record) xs
    where res = acc + x

parse :: String -> Int
parse (x : xs) = (if x == '+' then id else negate) (read xs :: Int)
