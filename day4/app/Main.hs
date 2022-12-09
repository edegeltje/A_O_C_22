module Main where

import Data.List.Split

-- splitOneOf :: Eq a => [a] -> [a] -> [[a]]
-- splitOneOf = undefined

main :: IO ()
main = interact $ show . puzzle1 . lines

puzzle1 :: [String] -> Int
puzzle1 = length . filter overlap . map (toSpecial . map (toSpecial . map read . splitOneOf "-") . splitOneOf ",")

type Low = Int 
type High = Int
type Sec = (Low,High)
type Paires = (Sec,Sec)

toSpecial :: [a] -> (a,a)
toSpecial (a:b:_) = (a,b)

overlap :: Paires -> Bool
overlap (p,q) = overlap' p q || overlap' q p

overlap' :: Sec -> Sec -> Bool
overlap' (a,b) (c,d) = a <= c && d <= b

