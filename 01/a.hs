module Day1A where

main :: IO ()
main = do
    text <- readFile "input.txt"
    print $ sum $ map parse $ lines text
        where
            parse (x : xs) = (if x == '+' then id else negate) (read xs :: Int)
