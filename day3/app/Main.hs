module Main where

import Data.List.Split

import Data.List
import Data.Maybe

main :: IO ()
main = interact (show . sum . puzzle2)

puzzle1 :: String -> [Int]
puzzle1 = map (sum . map toPriority . nub . uncurry intersect . splithalfway) . lines


puzzle2 :: String -> [Int]
puzzle2 = map (sum . nub . tripleIntersect) . chunksOf 3 . map (map toPriority) . lines

tripleIntersect :: Eq a => [[a]] -> [a]
tripleIntersect [a,b,c] = a `intersect` b `intersect` c
tripleIntersect _ = undefined

splithalfway :: [a] -> ([a],[a])
splithalfway as = splitAt (length as `div` 2) as

type Compartment = [Char]

type Rucksack = (Compartment,Compartment)

toPriority :: Char -> Int
toPriority = (+1) . fromJust . flip elemIndex (['a'..'z']++['A'..'Z'])