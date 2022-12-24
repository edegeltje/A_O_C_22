module Main where

import Data.List (nub)
import Control.Monad.State

main :: IO ()
main = interact (show . length . nub . trail . flip execState startState . sequence . map (action . readAction) . lines)

data Rope = Rope {
    knots :: [Point],
    trail :: Trail
}
    deriving (Eq, Show)

type Point = (Int,Int)

puzzle1 :: Int
puzzle1 = 2
puzzle2 :: Int
puzzle2 = 10

startState :: Rope
startState = Rope (replicate puzzle2 (0,0)) [(0,0)]

type Trail = [Point]

getTrail :: State Rope Trail
getTrail = do
    trail <$> get

getKnots :: State Rope [Point]
getKnots = do
    knots <$> get

putTrail :: Trail -> State Rope ()
putTrail t = do
    rope <- get
    put rope {trail =t}

getKnot :: Int -> State Rope Point
getKnot n = do
    knots <- getKnots
    return $ knots !! n 

putKnot :: Point -> Int -> State Rope ()
putKnot p n = do
    knots <- getKnots
    let (a,b:bs) = splitAt n knots
    putKnots $ a ++ (p:bs)


putKnots :: [Point] -> State Rope ()
putKnots ks = do
    rope <- get
    put rope {knots = ks}

pushTrail :: Point -> State Rope ()
pushTrail p = do
    trail <- getTrail
    putTrail (p:trail) 

getTip :: State Rope Point
getTip = getKnot 0

getEnd :: State Rope Point
getEnd = do
    last <$> getKnots

putTip :: Point -> State Rope ()
putTip = flip putKnot 0

putEnd :: Point -> State Rope ()
putEnd p = do
    n <- length <$> getKnots
    putKnot p (n-1)

readAction :: String -> (Char,Int)
readAction (d:' ':n) = (d,read n)
action :: (Char,Int) -> State Rope ()
action (_,0) = return ()
action (d,n) = do
    moveHead d
    l <- length <$> getKnots
    sequence [tensionRopePiece i | i <- [0..(l-2)]]
    registerMove
    action (d,n - 1)

add:: Point -> Point -> Point
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

difference :: Point -> Point -> Point
difference (x1,y1) (x2,y2) = (x1-x2,y1-y2)

moveHead :: Char -> State Rope ()
moveHead 'U' = do
    h <- getTip
    putTip $ add h (0,1)
moveHead 'D' = do
    h <- getTip
    putTip $ add h (0,-1)
moveHead 'L' = do
    h <- getTip
    putTip $ add h (-1,0)
moveHead 'R' = do
    h <- getTip
    putTip $ add h (1,0)

tensionRopePiece :: Int -> State Rope ()
tensionRopePiece n = do
    h <- getKnot n
    t <- getKnot (n+1)
    let newdif = case difference t h of
            (2,2) -> (1,1)
            (2,-2) -> (1,-1)
            (-2,2) -> (-1,1)
            (-2,-2) -> (-1,-1)
            (2,_) -> (1,0)
            (-2,_) -> (-1,0)
            (_,2) -> (0,1)
            (_,-2) -> (0,-1)
            a -> a
    putKnot (add h newdif) (n+1)

registerMove :: State Rope ()
registerMove = getEnd >>= pushTrail