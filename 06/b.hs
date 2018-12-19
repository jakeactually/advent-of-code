import Control.Monad
import Data.List
import Data.Map.Strict (fromListWith, toList)

type Point = (Int, Int)

main :: IO ()
main = do
  text <- readFile "input.txt"
  let coords = map parseCoord $ lines text
  let (xs, ys) = (map fst coords, map snd coords)
  let (minX, minY) = (minimum xs, minimum ys)
  let (maxX, maxY) = (maximum xs, maximum ys)
  let region = do
      x <- [minX..maxX]
      y <- [minY..maxY]
      let total = sum $ map (dist (x, y)) coords
      guard $ total < 10000
      return total
  print $ length region

parseCoord :: String -> (Int, Int)
parseCoord str = (read x, read y)
  where
    (x, _ : y) = break (== ',') str

dist :: Point -> Point -> Int
dist (a, b) (c, d) = abs (a - c) + abs (b - d)
