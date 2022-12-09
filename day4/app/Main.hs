module Main where

import Data.List.Split

-- splitOneOf :: Eq a => [a] -> [a] -> [[a]]
-- splitOneOf = undefined

main :: IO ()
main = interact $ show . length . filter overlap . map getpair . lines

getpair :: String -> Paires
getpair = toSpecial . map (toSpecial . map read . splitOneOf "-") . splitOneOf ","

type Low = Int 
type High = Int
type Sec = (Low,High)
type Paires = (Sec,Sec)

toSpecial :: [a] -> (a,a)
toSpecial (a:b:_) = (a,b)

overlap :: Paires -> Bool
overlap (p,q) = overlap' p q -- || overlap' q p -- the part beginning with || is only for problem 1 

overlap' :: Sec -> Sec -> Bool
-- overlap' (a,b) (c,d) = a <= c && d <= b --solution for problem 1
overlap' (a,b) (c,d) = a <= d && c <= b -- solution for problem 2
