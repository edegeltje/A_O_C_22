module Main where

import Data.List.Split

main :: IO ()
main = interact (undefined . map decodeMonkey . splitInMonkeys)

data Monkey = Monkey {
    number :: Int,
    items :: [Int],
    action :: Int -> Int,
    test :: Int,
    onSuccess :: Int,
    onFailure :: Int}


splitInMonkeys :: String -> [String]
splitInMonkeys = splitOn "\n\n"

parseAction :: String -> Int -> Int
parseAction pickle x = case (head $ drop 23 pickle) of
    '+' -> (+)
    '-' -> (-)
    '*' -> (*)
        x $ case (drop 25 pickle) of
            'o':'l':'d':_ -> x
            b -> (read :: String -> Int) b


decodeMonkey :: String -> Monkey
decodeMonkey pickle = Monkey {
    number = read $ reverse $ tail $ reverse $ drop 7 (lines pickle !! 0),
    items =  read $ ('[':) $ (++ "]") $ drop 18 (lines pickle !! 1),
    action = parseAction $ (lines pickle !! 2),
    test = read $ drop 21 (lines pickle !! 3),
    onSuccess = read $ drop 29 (lines pickle !! 4),
    onFailure = read $ drop 30 (lines pickle !! 5)
}

