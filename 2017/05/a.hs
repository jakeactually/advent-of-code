import Data.List

main :: IO ()
main = mapM_ putStrLn $ steps 0 numbers

steps :: Int -> [Int] -> [String]
steps i is = if i >= 0 && i <= 5
  then show is : steps (i + is !! i) (update (+1) i is)
  else []

update :: (a -> a) -> Int -> [a] -> [a]
update f i xs = let (as, b:bs) = splitAt i xs in as ++ f b : bs

numbers :: [Int]
numbers = [0, 3, 0, 1, -3]
