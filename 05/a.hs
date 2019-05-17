module Day5A where

import Data.Char

main :: IO ()
main = do
  text <- readFile "input.txt"
  print $ length $ solve "" $ filter isAlpha $ text

solve :: String -> String -> String
solve acc (c1 : c2 : cs) = if abs (ord c1 - ord c2) == 32
  then case acc of
    (a : as) -> solve as (a : cs)
    [] -> solve acc cs
  else solve (c1 : acc) (c2 : cs)
solve acc (c : cs) = reverse $ c : acc
solve acc [] = reverse $ acc
