module Day2B where

main :: IO ()
main = do
    text <- readFile "input.txt"
    let lines' = lines text
    print $ filter correct $ lines' >>= zip lines' . repeat

correct :: (String, String) -> Bool
correct (str1, str2) = length (filter (uncurry (/=)) $ zip str1 str2) == 1
