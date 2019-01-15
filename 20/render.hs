import Data.Array ((//), elems, listArray)
import Data.Char
import Data.Map (Map, assocs, empty, insert, keys)

data Expr = Character Char | Group [[Expr]]
    deriving Show

main :: IO ()
main =  do
    text' <- readFile "input.txt"
    let text = init $ tail text'
    let dic = run (0, 0) empty $ snd $ parseTokens [] text
    let ks = keys dic
    let (ys, xs) = (map fst ks, map snd ks)
    let (ly, lx) = (minimum ys - 1, minimum xs - 1)
    let (hy, hx) = (maximum ys + 1, maximum xs + 2)
    let base = listArray ((ly, lx), (hy, hx)) $ repeat '#'
    let arr = base // (((0, 0), 'X') : map (\y -> ((y, hx), '\n')) [ly..hy] ++ assocs dic)
    writeFile "cache.txt" $ elems arr
    print (abs ly, abs lx) -- offset

run :: (Int, Int) -> Map (Int, Int) Char -> [Expr] -> Map (Int, Int) Char
run pos dic (e : es) = case e of
    Character chr -> let (newPos, newDic) = eval chr pos dic in run newPos newDic es
    Group exprs -> run pos (foldl (run pos) dic exprs) es
run pos dic [] = dic

eval :: Char -> (Int, Int) -> Map (Int, Int) Char -> ((Int, Int), Map (Int, Int) Char)
eval chr (y, x) dic = case chr of
    'N' -> (,) (y - 2, x) $ insert (y - 1, x) '-' $ insert (y - 2, x) '.' dic
    'E' -> (,) (y, x + 2) $ insert (y, x + 1) '|' $ insert (y, x + 2) '.' dic
    'S' -> (,) (y + 2, x) $ insert (y + 1, x) '-' $ insert (y + 2, x) '.' dic
    'W' -> (,) (y, x - 2) $ insert (y, x - 1) '|' $ insert (y, x - 2) '.' dic

parseChar :: [Char] -> ([Char], Maybe Expr)
parseChar (b : buff) = if not $ isAlpha b
    then (buff, Nothing)
    else (buff, Just $ Character b)
parseChar _ = ([], Nothing)

parseTokens :: [Expr] -> [Char] -> ([Char], [Expr])
parseTokens acc buff = if null buff then ([], acc) else
    case parseChar buff of
        (cont, Just chr) -> parseTokens (acc ++ [chr]) cont
        _ -> case parseBranch buff of
            (cont1, Just branch) -> parseTokens (acc ++ [branch]) cont1
            _ -> (buff, acc)

parseBranch :: [Char] -> ([Char], Maybe Expr)
parseBranch buff = case buff of
    ('(' : bs) -> parseOptions bs []
    _ -> (buff, Nothing)

parseOptions :: [Char] -> [[Expr]] ->  ([Char], Maybe Expr)
parseOptions buff acc = case cont of
    ('|' : ')' : cs) -> (cs, Just $ Group $ next ++ [[]])
    ('|' : cs) -> parseOptions cs next
    (')' : cs) -> (cs, Just $ Group next)
    _ -> ([], Just $ Group acc)
    where
        (cont, tokens) = parseTokens [] buff
        next = acc ++ [tokens]
