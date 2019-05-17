module Day14A where

import Data.Sequence as S

main :: IO ()
main = do
    print $ cook 0 1 $ empty |> 3 |> 7

cook :: Int -> Int -> Seq Int -> Seq Int
cook i j seq = if len == 10 + 84601
    then S.drop 84601 seq
    else cook (ni + recipe1 + 1) (nj + recipe2 + 1) newSeq
    where
        len = S.length seq
        ni = mod i len
        nj = mod j len
        recipe1 = index seq ni
        recipe2 = index seq nj
        r = recipe1 + recipe2
        newSeq = if r < 10 then seq |> r else seq |> div r 10 |> mod r 10
