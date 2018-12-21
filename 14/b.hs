import Data.Array.IO
import Data.Array.MArray

main :: IO ()
main = do
    arr <- newListArray (0, 100000000) [3, 7]
    key <- newListArray (0, 5) [0,8,4,6,0,1]
    print =<< cook arr key Nothing 2 0 0 1

cook :: IOUArray Int Int -> IOUArray Int Int -> Maybe Int -> Int -> Int -> Int -> Int -> IO Int
cook arr key maybeR2 len hits i j = if hits > 5
    then return $ len - 6
    else case maybeR2 of
        Just r2 -> do
            writeArray arr len r2
            hit <- readArray key hits
            newHits <- if hit == r2
                then return $ hits + 1
                else (\hit0 -> if hit0 == r2 then 1 else 0) <$> readArray key 0
            cook arr key Nothing (len + 1) newHits i j
        Nothing -> do
            let ni = mod i len
            let nj = mod j len
            recipe1 <- readArray arr ni
            recipe2 <- readArray arr nj
            let r = recipe1 + recipe2

            let r1 = if r < 10 then r else div r 10
            let r2 = if r < 10 then Nothing else Just $ mod r 10
            writeArray arr len r1
            hit <- readArray key hits
            let newHits = if hit == r1 then hits + 1 else 0
            cook arr key r2 (len + 1) newHits (ni + recipe1 + 1) (nj + recipe2 + 1)
