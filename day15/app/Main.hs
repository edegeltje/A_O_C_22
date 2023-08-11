module Main where

main :: IO ()
main = interact $ show . puzzle2 . map (parseSensor . words) . lines
-- ####B######################
-- ####B######################
yinput :: Int
yinput = 2000000
ytest :: Int
ytest = 10
y :: Int
y = yinput
type Point = (Int,Int)
data Sensor = Sensor { 
  position::Point,
  beaconDistance::Int
  } deriving Show


parseSensor :: [String] -> Sensor
parseSensor s= Sensor {
  position=(x,y),
  beaconDistance = distance (x,y) (
    read $ init $ drop 2 $ s !! 8,
    read $ drop 2 $ s !! 9
    )} where
      x = read $ init $ drop 2 $ s!!2
      y = read $ init $ drop 2 $ s!!3


distance :: Point -> Point -> Int
distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1 - y2)

puzzle1 :: [Sensor] -> Int
puzzle1 sensors= length $ filter (\x -> cantContainBeacon (x,10) sensors) 
  [minimum $ map (maxLeft 10) sensors..maximum $ map (maxRight 10) sensors]

test1 :: [Sensor] -> String 
test1 sensors = map (\x -> showContents (x,y) sensors) 
  [minimum $ map (maxLeft y) sensors..maximum $ map (maxRight y) sensors]

puzzle2 :: [Sensor] -> Int
puzzle2 sensors = (\(x,y) -> 4000000 * x + y) $ puzzle2' sensors (0,0)

puzzle2' :: [Sensor] -> Point -> Point
puzzle2' sensors p@(x,y) = case closestSensor sensors p of
  Nothing -> p
  Just s -> if nextX s y > 4000000
    then puzzle2' sensors (0,y+1)
    else puzzle2' sensors (nextX s y, y)

nextX :: Sensor -> Int -> Int
nextX s y = fst (position s) + beaconDistance s + 1 - abs (snd (position s) - y)

closestSensor :: [Sensor] -> Point -> Maybe Sensor
closestSensor [] p = Nothing
closestSensor (s:sensors) p | beaconDistance s >= distance p (position s) = Just s
                            | otherwise = closestSensor sensors p

cantContainBeacon :: Point -> [Sensor] -> Bool
cantContainBeacon p = (`elem` "#S") . showContents p

showContents :: Point -> [Sensor] -> Char
showContents p [] = '.'
showContents p (s:ss) | p == position s = 'S'
                      | distance p (position s) <= beaconDistance s = '#'
                      | otherwise = showContents p ss 

maxLeft :: Int -> Sensor -> Int
maxRight :: Int -> Sensor -> Int
maxLeft y s@(Sensor (xs,ys) _) = xs - beaconDistance s + abs (ys-y) -1
maxRight y s@(Sensor (xs,ys) _) = xs + beaconDistance s - abs (ys-y) + 1

