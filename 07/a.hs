module Day7A where

import Data.Char (isUpper)
import Data.List (null)
import Data.Map as M (Map, (!), delete, fromList, keys, insertWith)
import Data.Set as S (Set, empty, insert, member)

main :: IO ()
main = do
  text <- readFile "input.txt"
  let edges = map parseEdge $ lines text
  let initialGraph = M.fromList $ map (\x -> (x, []) ) ['A'..'Z']
  let graph = foldl (\dic (k, v) -> insertWith (++) k v dic) initialGraph edges
  let consumed = empty :: Set Char
  print $ trySolve "" consumed graph

parseEdge :: String -> (Char, [Char])
parseEdge str = (b, [a])
  where
    up = filter isUpper str
    (_ : a : b : _) = up

trySolve :: [Char] -> Set Char -> Map Char [Char] -> [Char]
trySolve acc consumed graph = let as = available consumed graph in
  if null as
  then acc
  else solve as acc consumed graph

available :: Set Char -> Map Char [Char] -> [Char]
available consumed graph = filter f $ keys graph
  where f k = all (`member` consumed) (graph ! k)

solve :: [Char] -> [Char] -> Set Char -> Map Char [Char] -> [Char]
solve as acc consumed graph = trySolve newAcc newConsumed newGraph
  where
    a = minimum as
    newAcc = acc ++ [a]
    newConsumed = insert a consumed
    newGraph = delete a graph
