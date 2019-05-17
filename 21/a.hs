module Day21A where
    
import Data.Bits

main :: IO ()
main = print $ program 0 65536 3730679

program :: Int -> Int -> Int -> (Int, Int)
program iter px3 px4 =
    if 256 > px3
        then (px3, x4)
        else program (iter + 1) (div px3 256) x4
    where
        x5 = px3 .&. 255
        x4 = (((px4 + x5) .&. 16777215) * 65899) .&. 16777215
