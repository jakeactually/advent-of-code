module Day11Solve where

import Data.List
import Data.Function

data Record = Record 
    {   pos :: (Int, Int)
    ,   size :: Int
    ,   total :: Int
    } deriving (Show, Eq, Read)

instance Ord Record where
    compare = compare `on` total

main :: IO ()
main = do
    record <- foldl' max (Record (0, 0) 0 0) . map read . lines <$> readFile "log.txt"
    print record
