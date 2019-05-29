module Day24A where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Ord
import Text.Parsec

data GroupType = Inmune | Infection
    deriving (Show, Eq)

data Group = Group
    { gid :: Int
    , groupType :: GroupType
    , units :: Int
    , hp :: Int
    , weaknesses :: [String]
    , inmunities :: [String]
    , attack :: Int
    , attackType :: String
    , initiative :: Int
    } deriving Show

main :: IO ()
main = do
    text <- readFile "input.txt"
    let Right groups = parse infoParser "" text
    let boosted = map (\g -> if groupType g == Inmune then g { attack = attack g + 55 } else g) groups
    let dic = M.fromList $ zip [0..] boosted
    end <- foldM (\d _ -> loop d) dic [0..3500]
    print $ all ((==) Inmune . groupType) $ M.elems end
    print $ sum $ map units $ M.elems end

loop :: M.Map Int Group -> IO (M.Map Int Group)
loop dic = do
    let sorted = sortBy (comparing $ \i -> Down (power $ dic M.! i, initiative $ dic M.! i)) $ M.keys dic
    let sel = selection dic dic sorted
    newDic <- battles (sortBy (comparing $ \(i, _) -> Down $ initiative $ dic M.! i) sel) dic    
    putStrLn ""
    return $ M.filter (\g -> units g > 0) newDic   

battles :: [(Int, Int)] -> M.Map Int Group -> IO (M.Map Int Group)
battles bs dic = foldM (\d (gid1, gid2) -> kill gid1 gid2 d) dic bs

kill :: Int -> Int -> M.Map Int Group -> IO (M.Map Int Group)
kill gid1 gid2 dic = do
    putStrLn $ logBattle g1 g2 d killed
    return $ M.insert gid2 (g2 { units = units g2 - killed }) dic
    where
        g1 = dic M.! gid1
        g2 = dic M.! gid2
        d = damage g1 g2
        killed = min (units g2) (div d $ hp g2)

logBattle :: Group -> Group -> Int -> Int -> String
logBattle g1 g2 damage killed = unwords
    [ show (groupType g1)
    , show (gid g1)
    --, show (units g1)
    , show (groupType g2)
    , show (gid g2)
    --, show (units g2)
    , show damage
    , show killed
    ]

selection :: M.Map Int Group -> M.Map Int Group -> [Int] -> [(Int, Int)]
selection world state (i : is) = case select world state i of
    Just target -> [(i, target)] ++ selection world (M.delete target state) is
    Nothing -> selection world state is
selection _ _ [] = []

select :: M.Map Int Group -> M.Map Int Group -> Int -> Maybe Int
select world state gid = if null enemies || damage g1 (snd mx) <= 0
    then Nothing
    else Just $ fst mx
    where
        g1 = world M.! gid
        enemies = filter (\(_, g2) -> groupType g1 /= groupType g2) $ M.assocs state
        mx = maximumBy (comparing $ \(_, g2) -> (damage g1 g2, power g2, initiative g2)) enemies

damage :: Group -> Group -> Int
damage g1 g2 =
    if elem (attackType g1) (weaknesses g2) then power g1 * 2
    else if elem (attackType g1) (inmunities g2) then 0
    else power g1

power :: Group -> Int
power (Group { units = u, attack = a }) = u * a

-- Parser

infoParser :: Parsec String () [Group]
infoParser = do
    inmunes <- inmuneParser
    char '\n'
    infections <- infectionParser
    return (inmunes ++ infections)

inmuneParser :: Parsec String () [Group]
inmuneParser = do
    string "Immune System:\n"
    groups <- groupParser Inmune `endBy1` char '\n'
    return $ zipWith (\g i -> g { gid = i }) groups [1..]

infectionParser :: Parsec String () [Group]
infectionParser = do
    string "Infection:\n"
    groups <- groupParser Infection `endBy1` char '\n'    
    return $ zipWith (\g i -> g { gid = i }) groups [1..]

groupParser :: GroupType -> Parsec String () Group
groupParser groupType = do
    units <- numberParser
    string " units each with "
    hp <- numberParser
    string " hit points "
    (weaknesses, inmunities) <- option ([], []) $ attsParser
    string "with an attack that does "
    attack <- numberParser
    char ' '
    attackType <- wordParser
    string " damage at initiative "
    initiative <- numberParser
    return $ Group 0 groupType units hp weaknesses inmunities attack attackType initiative

attsParser :: Parsec String () ([String], [String])
attsParser = do
    char '('
    determinant <- wordParser
    atts1 <- string " to " >> listParser
    atts2 <- option [] $ string "; " >> wordParser >> string " to " >> listParser
    string ") "
    return $ if determinant == "weak" then (atts1, atts2) else (atts2, atts1)

numberParser :: Parsec String () Int
numberParser = read <$> many1 digit

wordParser :: Parsec String () String
wordParser = many1 letter

listParser :: Parsec String () [String]
listParser = wordParser `sepBy` (string ", ")
