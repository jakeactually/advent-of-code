import Data.List
import Data.Function
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char

data Vec3 = Vec3 Int Int Int
    deriving Show

data Nano = Nano
    { pos :: Vec3
    , radius :: Int
    } deriving Show

data Cube = Cube
    { cpos :: Vec3
    , size :: Int
    } deriving Show

data Contact = Contact
    { full :: Bool
    , coord :: Vec3
    , amount :: Int
    } deriving Show

main :: IO ()
main = do
    text <- readFile "input.txt"
    let nanos = map parseNano $ lines text
    passes nanos [Vec3 (-50000000) (-50000000) (-50000000)] 30

passes :: [Nano] -> [Vec3] -> Int -> IO ()
passes nanos offsets power = if power > 0
    then
        let
            info = offsets >>= pass nanos power
            mx = maximum $ map amount info
            mxs = take 5 $ filter ((== mx) . amount) info
            mn = minimumBy (compare `on` manhathan (Vec3 0 0 0) . coord) mxs
        in
            print mn >>
            passes nanos (map coord mxs) (power - 1)
    else
        return ()

pass :: [Nano] -> Int -> Vec3 -> [Contact]
pass nanos power offset = info
    where
        g = grid offset power
        info = mapMaybe (`contact` nanos) g

grid :: Vec3 -> Int -> [Cube]
grid (Vec3 ox oy oz) power = do
    x <- dimension
    y <- dimension
    z <- dimension
    return $ Cube (Vec3 (ox + x) (oy + y) (oz + z)) cell
    where
        size = 2 ^ power
        cell = div size 2
        dimension = [0, cell .. size - cell]

contact :: Cube -> [Nano] -> Maybe Contact
contact cube nanos = let is = filter (`touches` cube) nanos in
    if not $ null is
        then Just $ Contact (all (insideNano cube) is) (cpos cube) (length is)
        else Nothing

touches :: Nano -> Cube -> Bool
touches (Nano (Vec3 x1 y1 z1) r) (Cube (Vec3 x2 y2 z2) s) = not
     $ x1 - r >= x2 + s
    || x2 >= x1 + r
    || y1 - r >= y2 + s
    || y2 >= y1 + r
    || z1 - r >= z2 + s
    || z2 >= z1 + r

insideNano :: Cube -> Nano -> Bool
insideNano cube nano = cube `inside` subCube nano

inside :: Cube -> Cube -> Bool
inside (Cube (Vec3 x1 y1 z1) s1) (Cube (Vec3 x2 y2 z2) s2) =
    x1 >= x2 && x1 <= x2 + s2 &&
    y1 >= y2 && y1 <= y2 + s2 &&
    z1 >= z2 && z1 <= z2 + s2

subCube :: Nano -> Cube
subCube (Nano (Vec3 x y z) r) = Cube (Vec3 nx ny nz) size
    where
        offset = div r 2
        nx = x - offset
        ny =  y - offset
        nz = z - offset
        size = r + 1 - mod r 2

-- Parse

parseNano :: String -> Nano
parseNano str = let Right nano = parse nanoParser "" str in nano

nanoParser :: Parsec String () Nano
nanoParser = do
    string "pos=<"
    [x, y, z] <- numberParser `sepBy` (char ',')
    string ">, r="
    r <- numberParser
    return $ Nano (Vec3 x y z) r

numberParser :: Parsec String () Int
numberParser = read <$> many (digit <|> char '-')

manhathan :: Vec3 -> Vec3 -> Int
manhathan (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    abs (x1 - x2) +
    abs (y1 - y2) +
    abs (z1 - z2)
