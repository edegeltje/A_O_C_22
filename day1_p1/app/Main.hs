module Main where

import Data.List
import Data.List.Split

-- splitOn :: Eq a => [a] -> [a] -> [[a]]
-- splitOn = undefined

n = 1
-- for p1d1 use n = 1, for p2d1 use n = 3


main :: IO ()
main = interact (show . sum . take n . reverse . sort . textToElves)


type Elf = Int

textToElves :: String -> [Elf]
textToElves = map (sum . map (read :: String -> Int)) . splitOn [""] . lines
