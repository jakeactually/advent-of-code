module Day9A where

data Meta = Meta
  { players :: Int
  , total :: Int
  }

main :: IO ()
main = print $ maximum $ marble meta 1 [] rMarbles scores
  where
    meta = Meta 413 71082
    rMarbles = [0]
    scores = take (players meta) $ repeat 0

marble :: Meta -> Int -> [Int] -> [Int] -> [Int] -> [Int]
marble meta turn lMarbles rMarbles scores = if turn > total meta
  then scores
  else if mod turn 23 == 0
    then
      let
        newLMarbles = rMarbles ++ lMarbles
        (l1, m1 : m2 : l2) = splitAt (length newLMarbles - 8) newLMarbles
        scoreIndex = mod turn $ players meta
        (s1, s : s2) = splitAt scoreIndex scores
        newScores = s1 ++ (s + turn + m1) : s2
      in
        marble meta (turn + 1) (l1 ++ [m2]) l2 newScores
    else
      case rMarbles of
        (m : ms) -> marble meta (turn + 1) (lMarbles ++ [m, turn]) ms scores
        [] -> marble meta turn [] lMarbles scores
        