import Data.Char (isUpper, ord)
import Data.List ((\\), null, partition, sort)
import Data.Map as M (Map, (!), delete, fromList, insertWith, keys)
import Data.Set as S (Set, empty, insert, member)

main :: IO ()
main = do
  text <- readFile "input.txt"
  let edges = map parseEdge $ lines text
  let initialGraph = M.fromList $ map (\x -> (x, []) ) ['A'..'Z']
  let graph = foldl (\dic (k, v) -> insertWith (++) k v dic) initialGraph edges
  let consumed = empty :: Set Char
  print $ trySolve 0 [] "" consumed graph

parseEdge :: String -> (Char, [Char])
parseEdge str = (b, [a])
  where
    up = filter isUpper str
    (_ : a : b : _) = up

trySolve :: Int -> [(Char, Int)] -> [Char] -> Set Char -> Map Char [Char] -> (Int, [Char])
trySolve secs tasks acc consumed graph =
  let
    as = available consumed graph
  in
    if null as
      then (secs, acc)
      else
        let
          newTasks = tasks ++ map toTask (sort $ as \\ map fst tasks)
        in
          solve secs newTasks acc consumed graph

available :: Set Char -> Map Char [Char] -> [Char]
available consumed graph = filter f $ keys graph
  where f k = all (`member` consumed) (graph ! k)

toTask :: Char -> (Char, Int)
toTask char = (char, ord char - 4)

solve :: Int -> [(Char, Int)] -> [Char] -> Set Char -> Map Char [Char] -> (Int, [Char])
solve secs tasks acc consumed graph =
  let
    (queue, rest) = splitAt 5 tasks
    worked = map (\(char, count) -> (char, count - 1)) queue
    (ready, pending) = partition ((== 0) . snd) worked
  in
    if null ready
      then solve (secs + 1) (worked ++ rest) acc consumed graph
      else
        let
          readyChars = map fst ready
          newAcc = acc ++ readyChars
          newConsumed = foldr insert consumed readyChars
          newGraph = foldr delete graph readyChars
        in
          trySolve (secs + 1) (pending ++ rest) newAcc newConsumed newGraph
