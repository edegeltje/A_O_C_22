module Main where

import Data.List
import Data.Char


main :: IO ()
main = interact ( show . puzzle2 . lines)

puzzle1 :: [String] -> Int
puzzle1 = sum . map (length . filter getYesNo . checkBothWays) . transpose . map (checkBothWays . map No)

data YesNo a = Yes a | No a

changeToYes :: YesNo a -> YesNo a
changeToYes (Yes a) = Yes a
changeToYes (No a) = Yes a

changeToNo :: YesNo a -> YesNo a
changeToNo (Yes a) = No a
changeToNo (No a) = No a

getYesNo :: YesNo a -> Bool
getYesNo (Yes _) = True
getYesNo (No _) = False

setYesNo :: Bool -> YesNo a -> YesNo a
setYesNo b = if b then changeToYes else changeToNo

instance Functor YesNo where
    fmap f a = makeYesNo (getYesNo a) $ f $ getVal a

getVal :: YesNo a -> a
getVal (Yes a) = a
getVal (No a) = a

makeYesNo :: Bool -> a -> YesNo a
makeYesNo b = if b then Yes else No

makeBoolTable :: [[a]] -> [[YesNo a]]
makeBoolTable = map (map No)

orVisible :: Ord a => [YesNo a] -> [YesNo a]
orVisible (a:as) = changeToYes a: (orVisible' (getVal a) as)

orVisible'  :: Ord a => a -> [YesNo a] -> [YesNo a]
orVisible' _ [] = []
orVisible' bar (a:as) = setYesNo (bar < getVal a || getYesNo a) a : orVisible' (max bar $ getVal a) as

checkBothWays :: Ord a => [YesNo a] -> [YesNo a]
checkBothWays = reverse . orVisible . reverse . orVisible

roll :: [a] -> [a]
roll (a:as) = as ++ [a]

rollNTimes :: Int -> [a] -> [a]
rollNTimes n = flip (foldr (id)) [roll | i <- [1..n]]

shiftXY :: (Int,Int) -> [[a]] -> [[a]]
shiftXY (x,y) = rollNTimes y . map (rollNTimes x)



puzzle2 :: [String] -> Int 
puzzle2 = (\x -> maximum $ [ calculateValue (i,j) x| j <- [0..(length x - 1)], i <- [0..(length (x !! 0) - 1)] ] ) . map (map digitToInt)

getCoords :: (Int,Int) -> [[Int]] -> Int
getCoords (x,y) = (!! x) . (!! y)

surround :: Int -> [Int] -> [Int]
surround n = (n:) . (++[n])

checkElement :: (Int,Int) -> [[Int]] -> Int
checkElement coords forestMap = undefined

checkVisible :: Int -> [Int] -> Int
checkVisible _ [] = 0
checkVisible high (a:as) = 1 + 
    if (a<high) then checkVisible high as else 0

getFourDirections :: (Int,Int) -> [[Int]] -> [[Int]]
getFourDirections (x,y) forestMap = concat $ map removeCenter [splitAt x (forestMap !! y), splitAt y (transpose forestMap !! x)]

removeCenter :: ([a],[a])-> [[a]]
removeCenter (a,b:bs)=[ reverse a,bs]

getVisibles :: (Int,Int) -> [[Int]] -> [Int]
getVisibles coords forestMap = map (checkVisible (getCoords coords forestMap) ) $
  getFourDirections coords forestMap

calculateValue :: (Int,Int) -> [[Int]] -> Int
calculateValue coords forestMap = product $ getVisibles coords forestMap

