import Data.List

main :: IO ()
main = do
    text <- readFile "input.txt"
    print $ uncurry (*) $ checksum 0 0 $ map (group . sort) $ lines text

checksum :: Int -> Int -> [[String]] -> (Int, Int)
checksum twos threes (x : xs) = checksum
    (if any ((== 2) . length) x then twos + 1 else twos)
    (if any ((== 3) . length) x then threes + 1 else threes)
    xs
checksum twos threes [] = (twos, threes)
