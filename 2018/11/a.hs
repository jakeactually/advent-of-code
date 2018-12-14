import Control.Monad
import Data.Array.IO
import Data.List
import Data.Maybe

size, limit :: Int
size = 300 * 300 - 1
limit = 298 * 298 - 1

main :: IO ()
main = do
    arr <- newArray (0, size) 0 :: IO (IOUArray Int Int)
    mapM_ (\i -> writeArray arr i $ toPower i) [0..size]
    powers <- mapM (\i -> foldM (\acc j -> (acc +) <$> readArray arr j) 0 $ grid i) [0..limit]
    print $ maximum powers
    print $ toVec $ fromJust $ elemIndex (maximum powers) powers

toPower :: Int -> Int
toPower i = power
    where
        (x', y') = toVec i
        (x, y) = (x' + 1, y' + 1)
        rackId = x + 10
        start = rackId * y
        processed = (start + 6392) * rackId
        hundreds = if processed > 99 then div (mod processed 1000) 100 else 0
        power = hundreds - 5

toVec :: Int -> (Int, Int)
toVec i = (mod i 300, div i 300)

toInt :: (Int, Int) -> Int
toInt (x, y) = y * 300 + x

grid :: Int -> [Int]
grid i = let (x, y) = toVec i in
    [ toInt (x, y)
    , toInt (x + 1, y)
    , toInt (x + 2, y)

    , toInt (x, y + 1)
    , toInt (x + 1, y + 1)
    , toInt (x + 2, y + 1)

    , toInt (x, y + 2)
    , toInt (x + 1, y + 2)
    , toInt (x + 2, y + 2)
    ]
