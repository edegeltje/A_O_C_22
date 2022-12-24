module Main where

import Control.Monad.State

import Data.List.Split

main :: IO ()
main = interact (puzzle2 . reverse . registerHistory . 
    flip execState startState . sequence . map doAction . lines)

puzzle1 :: [Int] -> String
puzzle1 = show . sum . calculateSignalStrength . filterHistory

puzzle2 :: [Int] -> String
puzzle2 = unlines . chunksOf 40 . render

render :: [Int] -> String
render = zipWith (\a b -> if abs(a-b)<2 then '#' else '.') $ concat (repeat [0..39])


filterHistory = map head . chunksOf 40 . drop 19

calculateSignalStrength = zipWith (*) [20,60..]

startState :: GPU
startState = GPU [1]

data GPU = GPU {
    registerHistory :: [Int]
}

doAction:: String -> State GPU ()
doAction "noop" = noOp
doAction ('a':'d':'d':'x':' ':r) = addx (read r)

noOp :: State GPU ()
noOp = do
    r <- getRegister
    pushRegister r

addx :: Int -> State GPU ()
addx n = do
    noOp
    r <- getRegister
    pushRegister (r+n)

getRegister :: State GPU Int
getRegister = do
    head <$> registerHistory <$> get

pushRegister :: Int -> State GPU ()
pushRegister r = do
    gpu <- get
    put gpu{registerHistory = r:(registerHistory gpu) }

