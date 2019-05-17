module Day21B where

import Data.Bits
import Data.Function (on)
import Data.List (minimumBy)
import Data.Set (Set, empty, insert, member)

main :: IO ()
main =
    writeFile "log.txt" $
    unlines $
    map show $
    program empty 65536 3730679

program :: Set Int -> Int -> Int -> [(Int, Int)]
program set px3 px4 =
    if member x4 set
    then [(px3, x4)]
    else if 256 > px3
    then (px3, x4) : program newSet (x4 .|. 65536) 3730679
    else program set (div px3 256) x4
    where
        x5 = px3 .&. 255
        x4 = (((px4 + x5) .&. 16777215) * 65899) .&. 16777215
        newSet = insert x4 set

iterProgram :: Int -> Int -> Int -> [(Int, Int)]
iterProgram iter px3 px4 =
    if iter <= 0
    then [(px3, x4)]
    else if 256 > px3
    then (px3, x4) : iterProgram (iter - 1) (x4 .|. 65536) 3730679
    else iterProgram iter (div px3 256) x4
    where
        x5 = px3 .&. 255
        x4 = (((px4 + x5) .&. 16777215) * 65899) .&. 16777215
