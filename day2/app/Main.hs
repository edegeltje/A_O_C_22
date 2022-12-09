module Main where

main :: IO ()
main = interact (show . sum . map (matchPoints . guideToMatches) . lines)

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

toRPS :: Char -> RPS
toRPS 'A' = Rock
toRPS 'B' = Paper
toRPS 'C' = Scissors
toRPS 'X' = Rock
toRPS 'Y' = Paper
toRPS 'Z' = Scissors
toRPS _ = undefined

type Match = (RPS,RPS)

points :: RPS -> Integer
points Rock = 1
points Paper = 2
points Scissors = 3

matchPoints :: Match -> Integer
matchPoints (a,b) = calculate $ calculateRequiredMove (points a, points b)

calculate :: (Integer, Integer) -> Integer
calculate (a,b) = a + 3 * ((1 + a - b) `mod` 3)

guideToMatches :: String -> Match
guideToMatches [a,' ',b] = (toRPS a,toRPS b) 
guideToMatches _ = undefined

calculateRequiredMove :: (Integer,Integer) -> (Integer,Integer)
calculateRequiredMove (a,b) = (1 + mod (a+b) 3,a) -- for problem 1, change to (b,a)