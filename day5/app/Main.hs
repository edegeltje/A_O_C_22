module Main where

import Data.List.Split
import Data.List
import Control.Monad.State

-- splitOneOf :: Eq a => [a] -> [a] -> [[a]]
-- splitOneOf = undefined

main :: IO ()
main = interact (show . puzzle . lines)

puzzle = getOutput . uncurry execState . parseInput

testYard = parseYard

testActions = map parseAction

parseInput :: [String] -> (State Yard [()], Yard)
parseInput = (\[a,b] -> ( mapM (doAction . parseAction) b, parseYard a)) . splitOneOf [[]]

getOutput :: Yard -> String

getOutput y = evalState (mapM popCrate [1..length y]) y


doAction :: Action -> State Yard ()
doAction a@(Action n f t) = do
  crate <- mapM (const (popCrate f)) [1..n]
  let puzzle1 = crate
  let puzzle2 = reverse crate
  mapM_ (putCrate t) puzzle2

popCrate :: Int -> State Yard Char
popCrate n = do
  yard <- get
  let (head, (c:stack):tail) = splitAt (n-1) yard
  put (head ++ stack:tail)
  return c

putCrate :: Int -> Char -> State Yard ()
putCrate n c = do
  yard <- get
  let (head, stack:tail) = splitAt (n-1) yard
  put (head ++ (c:stack):tail)

parseYard :: [String] -> Yard
parseYard = map yeetSpaces . takeSecondsOfFour . transpose

takeSecondsOfFour :: [a] -> [a] -- take every second of four
takeSecondsOfFour (a:b:c:d:es) = b:takeSecondsOfFour es
takeSecondsOfFour (a:b:cs) = [b]
takeSecondsOfFour _ = undefined

takeSeconds :: [a] -> [a]
takeSeconds (a:b:cs) = b: takeSeconds cs
takeSeconds a = []

yeetSpaces :: String -> String
yeetSpaces (' ':cs) = yeetSpaces cs
yeetSpaces a = a

parseAction :: String -> Action
parseAction = listToAction . map (read :: String -> Int) . takeSeconds . splitOneOf " "

listToAction :: [Int] -> Action
listToAction [a,b,c] = Action a b c

type Stack = String

type Yard = [Stack]

data Action = Action {
  amount :: Int,
  origin :: Int,
  to :: Int}
  deriving (Eq, Show)