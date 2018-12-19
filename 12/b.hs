import Data.Map ((!?), Map, fromList)

data Rule = Rule State State State State State
  deriving (Eq, Show, Ord)

data State = On | Off
  deriving (Eq, Show, Ord)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let iLines = lines input
  let initial = drop 2 $ snd $ break (== ':') $ head iLines
  let len = length initial
  let plants = map parseState initial
  let rules = fromList $ map parseRule $ drop 2 iLines
  let gs = gens 100 rules plants
  let pairs = zip [100 * (-2) .. len + 100 * 2] gs
  print $ sum $ map fst $ filter ((== On) . snd) pairs

  let gs = gens 150 rules plants
  let pairs = zip [150 * (-2) .. len + 150 * 2] gs
  print $ sum $ map fst $ filter ((== On) . snd) pairs

  print $ 5691 + (150 - 100) * 62
  print $ 5691 + (50000000000 - 100) * 62

parseState :: Char -> State
parseState c = case c of
  '#' -> On
  '.' -> Off

parseRule :: String -> (Rule, State)
parseRule str = (Rule a b c d e, r)
  where
    (match, result) = break (== ' ') str
    [a, b, c, d, e] = map parseState match
    r = parseState $ last $ result

pad :: [State] -> [State]
pad states = Off : Off : states ++ [Off, Off]

gen :: Map Rule State -> [State] -> [State]
gen dic (a : b : c : d : e : rest) = case dic !? rule of
  Just state -> [state] ++ gen dic next
  Nothing -> [Off] ++ gen dic next
  where
    rule = Rule a b c d e
    next = (b : c : d : e : rest)
gen _ _ = []

gens :: Int -> Map Rule State -> [State] -> [State]
gens age dic states = if age == 0
  then states
  else gens (age - 1) dic $ gen dic $ pad $ pad states 

toChar :: State -> Char
toChar state = case state of
  On -> '#'
  Off -> '.'
