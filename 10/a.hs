import Data.Array ((//), listArray, elems)
import Data.Bits (shiftR)
import Data.ByteString as Bs (pack, writeFile)
import Data.Word (Word8)
import Numeric.Natural

width, height, colors, multipliedWidth, padding, row, bytes, size, time :: Int
width = 1000
height = 1000
colors = 3
multipliedWidth = width * colors
padding = let m = mod multipliedWidth 4 in if m == 0 then 0 else 4 - m
row = multipliedWidth + padding
bytes = row * height
size = bytes + 54
time = 10656

main :: IO ()
main = do
    text <- readFile "input.txt"
    let stars = map parseStar $ lines text
    let moved = map (move time) stars
    let body = elems $ listArray (0, bytes - 1) (replicate bytes 0) // (moved >>= toPixel)
    Bs.writeFile "img.bmp" $ pack $ header ++ body

parseStar :: String -> (Int, Int, Int, Int)
parseStar str = (int x, int y, int vx, int $ init vy)
    where
        (positionStr, velocityStr) = break (== '>') str
        position = tail $ dropWhile (/= '<') positionStr
        velocity = tail $ dropWhile (/= '<') velocityStr
        (x, _ : y) = break (== ',') position
        (vx, _ : vy) = break (== ',') velocity

int :: String ->  Int
int = read

move :: Int -> (Int, Int, Int, Int) -> (Int, Int)
move t (x, y, vx, vy) = (x + vx * t, y + vy * t)

-- https://en.wikipedia.org/wiki/BMP_file_format#Example_1

header :: [Word8]
header = let x >> y = fromIntegral $ shiftR x y in
    [   0x42, 0x4D,
        fromIntegral size, size >> 8, size >> 16, size >> 24,
        0x00, 0x00,
        0x00, 0x00,
        0x36, 0x00, 0x00, 0x00,
        0x28, 0x00, 0x00, 0x00,
        fromIntegral width, width >> 8, width >> 16, width >> 24,
        fromIntegral height, height >> 8, height >> 16, height >> 24,
        0x01, 0x00,
        0x18, 0x00,
        0x00, 0x00, 0x00, 0x00,
        fromIntegral bytes, bytes >> 8, bytes >> 16, bytes >> 24,
        0x13, 0x0B, 0x00, 0x00,
        0x13, 0x0B, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00
    ]

toPixel :: (Int, Int) -> [(Int, Word8)]
toPixel (x', y') = [(i, 255), (i + 1, 255), (i + 2, 255)]
    where
        x = max 0 $ min x' width - 1
        y = height - (max 0 $ min y' height - 1)
        i = y * row + x * colors
