module Day24A where

import Text.Parsec
import Text.Parsec.Char

data Group = Group
    { units :: Int
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
    print $ parse groupParser "" $ lines text !! 2

groupParser :: Parsec String () Group
groupParser = do
    units <- numberParser
    string " units each with "
    hp <- numberParser
    string " hit points ("
    determinant <- wordParser
    atts1 <- string " to " >> listParser
    atts2 <- option [] $ string "; " >> wordParser >> string " to " >> listParser
    let (weaknesses, inmunities) = if determinant == "weak" then (atts1, atts2) else (atts2, atts1)
    string ") with an attack that does "
    attack <- numberParser
    char ' '
    attackType <- wordParser
    string " damage at initiative "
    initiative <- numberParser
    return $ Group units hp weaknesses inmunities attack attackType initiative

numberParser :: Parsec String () Int
numberParser = read <$> many digit

wordParser :: Parsec String () String
wordParser = many letter

listParser :: Parsec String () [String]
listParser = wordParser `sepBy` (string ", ")
