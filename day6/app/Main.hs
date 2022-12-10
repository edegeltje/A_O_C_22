module Main where

import Data.List

main :: IO ()
main = interact (show . map (\x -> process (drop n x) (reverse $ take n x) n ) . lines)

n :: Int
n = 13 -- for puzzle 1, use n = 3, for puzzle 2, use n = 13

init' :: [a] -> [a]
init' = take n

--         input     buffer    count  result
process :: String -> String -> Int -> Int
process [] _ _ = 0
process (c:input) buffer count = if isUnique newBuffer then count+1
  else 
    process input newBuffer count+1
  where
    newBuffer = pushBuffer buffer c

pushBuffer :: String -> Char -> String
pushBuffer buffer c = c:init' buffer

isUnique :: String -> Bool
isUnique a = a == nub a