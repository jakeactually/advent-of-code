import Data.Array.IO
import Data.Array.MArray

main :: IO ()
main = do
    text <- readFile "input.txt"
    let points = getPoints text
    let maxX = maximum $ map snd points
    let maxY = maximum $ map fst points
    arr <- newArray ((0, 0), (maxY, maxX + 1)) '.' :: IO (IOUArray (Int, Int) Char)
    mapM_ (\p -> writeArray arr p '#') points
    mapM_ (\p -> writeArray arr p '\n') $ map (\y -> (y, maxX + 1)) [0..maxY]
    writeArray arr (0, 500) '+'
    pour arr maxY [] (0, 500)
    els <- getElems arr
    writeFile "output.txt" els
    print $ length $ filter (\c -> c == '~') els

getPoints :: String -> [(Int, Int)]
getPoints text = do
    line <- lines text
    let (first, _ : second) = break (== ',') line
    let firstNumber = read $ drop 2 first
    let (low, _ : _ : high) = break (== '.') $ drop 3 second
    secondNumber <- [read low .. read high] :: [Int]
    if head first == 'x'
        then return (secondNumber, firstNumber)
        else return (firstNumber, secondNumber)

pour :: IOUArray (Int, Int) Char -> Int -> [(Int, Int)] -> (Int, Int) ->  IO (Maybe Int)
pour arr maxY os (y, x) = if y >= maxY then return Nothing else do
    chr <- readArray arr (y + 1, x)
    if chr == '.'
    then writeArray arr (y + 1, x) '|' >> pour arr maxY ((y, x) : os) (y + 1, x)
    else if chr == '|' then return Nothing
    else do
        lm <- left arr maxY os (y, x)
        rm <- right arr maxY os (y, x)
        case (lm, rm) of
            (Just lx, Just rx) -> do
                mapM_ (\x' -> writeArray arr (y, x') '~') [lx..rx]
                pour arr maxY (tail os) (head os)
            _ -> return Nothing

left :: IOUArray (Int, Int) Char -> Int -> [(Int, Int)] -> (Int, Int) -> IO (Maybe Int)
left arr maxY os (y, x) = do
    chr <- readArray arr (y, x - 1)
    if chr == '.' || chr == '|'
        then do
            writeArray arr (y, x - 1) '|'
            chr <- readArray arr (y + 1, x - 1)
            if chr == '.'
                then pour arr maxY os (y, x - 1)
                else left arr maxY os (y, x - 1)
        else return $ Just x

right :: IOUArray (Int, Int) Char -> Int -> [(Int, Int)] -> (Int, Int) -> IO (Maybe Int)
right arr maxY os (y, x) = do
    chr <- readArray arr (y, x + 1)
    if chr == '.' || chr == '|'
        then do
            writeArray arr (y, x + 1) '|'
            chr <- readArray arr (y + 1, x + 1)
            if chr == '.'
                then pour arr maxY os (y, x + 1)
                else right arr maxY os (y, x + 1)
        else return $ Just x
