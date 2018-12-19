import Data.Char
import Data.Function
import Data.List

main :: IO ()
main = do
  text <- readFile "input.txt"
  let chars = filter isAlpha $ text
  let solved = map (solve "" . (`noChar` chars)) [65..90]
  let min = minimumBy (compare `on` length) solved
  print $ length $ min

noChar :: Int -> String -> String
noChar i = filter (\c -> c /= chr i && c /= chr (i + 32))

solve :: String -> String -> String
solve acc (c1 : c2 : cs) = if abs (ord c1 - ord c2) == 32
  then case acc of
    (a : as) -> solve as (a : cs)
    [] -> solve acc cs
  else solve (c1 : acc) (c2 : cs)
solve acc (c : cs) = reverse $ c : acc
solve acc [] = reverse $ acc
