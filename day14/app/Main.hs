{-# LANGUAGE TupleSections #-}
module Main where
import Control.Monad.State
import Data.List

data CaveContent = Rock | Air | Sand deriving (Eq)

instance Show CaveContent where
  show Rock = "#"
  show Air = "."
  show Sand = "+"

data CaveLayer = NewLayer {
  layer :: [CaveContent],
  xoffset :: Int
}
dropDots :: String -> String
dropDots [] = []
dropDots ('.':rs) = dropDots rs
dropDots other = other
instance Show CaveLayer where
  show a = dropDots . concatMap show $ layer a

replace :: Int -> v -> [v] -> [v]
replace 0 val (v:vs)= val:vs
replace i val (v:vs)= v:replace (i-1) val vs

putXOffset :: Int -> State CaveLayer ()
putXOffset x = do
  l <- get
  put l{xoffset=x}

getCaveContent :: Int -> State CaveLayer CaveContent
getCaveContent x = do 
  l <- gets layer
  xOffset <- gets xoffset
  let xIndex = x - xOffset
  if xIndex < 0 || length l <= xIndex then
    return Air
  else
    return $ l !! (x-xOffset)


putCaveContent :: Int -> CaveContent -> State CaveLayer ()
putCaveContent x c = do
  l <- get
  let cs = layer l
  let xOffset = xoffset l
  let xIndex = x - xOffset
  if xIndex < 0 then
    put $ NewLayer {layer = c:replicate (-1-xIndex) Air ++ cs, xoffset=x}
  else
    if xIndex >= length cs then
      put $ l{layer = cs++replicate (xIndex-length cs) Air ++[c]}
    else
      put $ l{layer = replace xIndex c cs}

data Cave = NewCave {
  layers :: [CaveLayer],
  yoffset :: Int
}

instance Show Cave where
  show c = unlines $ zipWith (++) leftPads $ zipWith (++) rowShows rightPads where
    xOffsets = map xoffset $ layers c
    minXOffset = minimum xOffsets
    leftPadLengths = map (flip (-) minXOffset) xOffsets
    lengths = map (length . layer) $ layers c
    rightEdges = zipWith (+) xOffsets lengths
    maxX = maximum rightEdges
    leftPads = map (`replicate` '.') leftPadLengths
    rightPadLengths = map (maxX -) rightEdges
    rightPads = map (`replicate` '.') rightPadLengths
    rowShows = map (concatMap show . layer) $ layers c

    

putYOffset :: Int -> State Cave ()
putYOffset y = do
  c <- get
  put c{yoffset=y}

getCaveLayer :: Int -> State Cave CaveLayer
getCaveLayer y = do 
  l <- gets layers
  yOffset <- gets yoffset
  let yIndex = y - yOffset
  if y < 0 || y >= length l then
    return NewLayer {layer = [Air], xoffset=500}
  else
    return $ l !! yIndex


putCaveLayer :: Int -> CaveLayer -> State Cave ()
putCaveLayer y l = do
  c <- get
  let ls = layers c
  let yOffset = yoffset c
  let yIndex = y - yOffset
  if yIndex < 0 then
    put c{layers = l:replicate (-1-yIndex) (NewLayer [Air] 500) ++ ls, yoffset = y}
  else
    if yIndex >= length ls then
      put c{layers=ls ++ replicate (yIndex - length ls) (NewLayer [Air] 500) ++ [l]}
    else
      put c{layers = replace yIndex l ls}

forCaveLayer :: Int -> State CaveLayer a -> State Cave a
forCaveLayer i action = do
  l <- getCaveLayer i
  let (a,s) = runState action l
  putCaveLayer i s
  return a

main :: IO ()
main = interact $ show . puzzle2 . lines

test1 :: Cave
test1 = execState (addLine ((500,1),(504,1))) NewCave{layers=[NewLayer{layer=[Air],xoffset=500}],yoffset=0}

test2 :: [(Integer, Integer)]
test2 =  zip [500..500] [1..3]

test3 :: [(Int, Int)]
test3 = fillLine ((500,1),(503,1))

test4 :: Cave
test4 = execState (addRock "498,4 -> 498,6 -> 496,6") NewCave{layers=[NewLayer{layer=[Air],xoffset=500}],yoffset=0}

puzzle2 :: [String] -> (Int,Cave)
puzzle2 c = runState (do
  parseRocks c
  y <- gets yoffset
  l <- gets (length . layers)
  let maxY = y + l
  dropSand2 (500,0) maxY
  l2 <- gets (length . layers)
  gets (sum . map (length . filter (==Sand) . layer) . layers)
  ) newCave

puzzle1 :: [String] -> (Int,Cave)
puzzle1 c = runState (do
  parseRocks c
  dropSand1 (500,0)
  y <- gets yoffset
  ls <- gets layers

  gets (sum . map (length . filter (==Sand) . layer) . layers)
  ) newCave

dropSand1 :: (Int,Int) -> State Cave ()
dropSand1 (x,y) = do
  minY <- gets yoffset
  ls <- gets layers
  let maxY = minY + length ls - 1
  if y >= maxY then return () else do
    a <- forCaveLayer (y+1) $ getCaveContent x
    if a == Air then dropSand1 (x,y+1) else do
      b <- forCaveLayer (y+1) $ getCaveContent (x-1)
      if b == Air then dropSand1 (x-1,y+1) else do
        c <- forCaveLayer (y+1) $ getCaveContent (x+1)
        if c == Air then dropSand1 (x+1,y+1) else do
          forCaveLayer y $ putCaveContent x Sand
          dropSand1 (500,0)

dropSand2 :: (Int,Int) -> Int -> State Cave ()
dropSand2 (x,y) maxy = do
  z <- forCaveLayer y $ getCaveContent x
  if z == Sand then return () else do
    if y >= maxy then (
      do forCaveLayer y $ putCaveContent x Sand
         dropSand2 (500,0) maxy
      ) else do
      a <- forCaveLayer (y+1) $ getCaveContent x
      if a == Air then dropSand2 (x,y+1) maxy else do
        b <- forCaveLayer (y+1) $ getCaveContent (x-1)
        if b == Air then dropSand2 (x-1,y+1) maxy else do
          c <- forCaveLayer (y+1) $ getCaveContent (x+1)
          if c == Air then dropSand2 (x+1,y+1) maxy else do
            forCaveLayer y $ putCaveContent x Sand
            dropSand2 (500,0) maxy

newCave :: Cave
newCave = NewCave{layers=[NewLayer{layer=[Air],xoffset=500}],yoffset=0}

parseRocks :: [String] -> State Cave ()
parseRocks = mapM_ addRock 

addRock :: String -> State Cave ()
addRock = mapM_ addLine . splitRockLines . splitRockCoords

other :: [a] -> [a]
other [] = []
other [a] = [a]
other (a:b:r) = a:other r

splitRockCoords :: String -> [(Int,Int)]
splitRockCoords = map (read . ('(':) . (++")")) . other . words

splitRockLines :: [(Int,Int)] -> [((Int,Int),(Int,Int))]
splitRockLines coords = zip (init coords) $ tail coords


addLine :: ((Int,Int),(Int,Int)) -> State Cave ()
addLine = mapM_ 
  (uncurry (flip forCaveLayer . flip putCaveContent Rock)) . fillLine

fillLine :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
fillLine ((x1,y1),(x2,y2)) | x1==x2 && y1 > y2 = map (x1,) [y2..y1]
                           | x1 == x2          = map (x1,) [y1..y2]
                           | y1==y2 && x1 > x2 = map (,y1) [x2..x1]
                           | otherwise = map (,y1) [x1..x2]