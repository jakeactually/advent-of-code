data Node = Node [Node] [Int]
    deriving Show

main :: IO ()
main = do
    text <- readFile "input.txt"
    let nums = map read $ words text :: [Int]
    print $ value $ fst $ parseNode nums

parseNode :: [Int] -> (Node, [Int])
parseNode (childrenAmount : metaAmount : rest) = (Node children meta, rest2)
    where
        (children, rest1) = parseNodes childrenAmount [] rest
        (meta, rest2) = splitAt metaAmount rest1

parseNodes :: Int -> [Node] -> [Int] -> ([Node], [Int])
parseNodes childrenLeft nodes stream = if childrenLeft == 0
    then (nodes, stream)
    else
        let (node, rest) = parseNode stream
        in parseNodes (childrenLeft - 1) (nodes ++ [node]) rest

value :: Node -> Int
value (Node children meta) = if len == 0
    then sum meta
    else foldl (\acc index -> if index < len then acc + value (children !! (index - 1)) else acc) 0 meta
        where len = length children
