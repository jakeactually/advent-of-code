module Day6A where

import Control.Monad
import Data.Function
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
  let plot = fromListWith (++) $ do
      x <- [minX..maxX]
      y <- [minY..maxY]
      let cs = close (x, y) 1000000 [] coords
      guard $ length cs == 1
      return $ (head cs, [(x, y)])
  let bound (x, y) = x == minX || x == maxX || y == minY || y == maxY
  let noInf (k, v) = not $ any bound v
  let valid = filter noInf $ toList $ plot
  let maxP = maximumBy (compare `on` length . snd) valid 
  print $ length $ snd maxP
  
parseCoord :: String -> (Int, Int)
parseCoord str = (read x, read y)
  where
    (x, _ : y) = break (== ',') str

close :: Point -> Int -> [Point] -> [Point] -> [Point]
close p d acc (x : xs) = case compare d d' of
  LT -> close p d acc xs
  EQ -> close p d (x : acc) xs
  GT -> close p d' [x] xs
  where
    d' = dist p x
close p d acc [] = acc

dist :: Point -> Point -> Int
dist (a, b) (c, d) = abs (a - c) + abs (b - d)
