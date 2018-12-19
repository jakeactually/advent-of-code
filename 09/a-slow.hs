data Meta = Meta
    { players :: Int
    , total :: Int
    }

main :: IO ()
main = print $ marble meta (turn + 1) marbles scores index len
    where
        meta = Meta 413 71082
        turn = 3
        marbles = [0, 2, 1, 3]
        scores = take (players meta) $ repeat 0
        index = 3
        len = 4

marble :: Meta -> Int -> [Int] -> [Int] -> Int -> Int -> Int
marble meta turn marbles scores index len = if turn == total meta + 1
    then maximum scores
    else if mod turn 23 == 0 
        then
            let
                newIndex = if index < 7 then len - 7 + index else index - 7
                (m1, m : m2) = splitAt newIndex marbles
                currentPlayer = mod turn $ players meta
                (s1, s : s2) = splitAt currentPlayer scores
                newScores = s1 ++ (s + turn + m) : s2
                newMarbles = m1 ++ m2
            in
                marble meta (turn + 1) newMarbles newScores newIndex (len - 1)
        else
            let
                newIndex = if index == len - 1 then 1 else index + 2
                (as, bs) = splitAt newIndex marbles
                newMarbles = as ++ turn : bs
            in
                marble meta (turn + 1) newMarbles scores newIndex (len + 1)
