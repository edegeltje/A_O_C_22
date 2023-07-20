module Main where
import Control.Monad.State
import Data.List

data Monkey = Monkey {
  items :: [Int],
  operation :: Int -> Int,
  test :: Int,
  true_monkey :: Int,
  false_monkey :: Int,
  inspections::Int
} 
type Monkeys = [Monkey]

monkeyTurn1 :: State Monkey [(Int,Int)]
monkeyTurn1 = do
  monke <- get
  op <- gets operation
  its <- gets items
  tru <- gets true_monkey
  fals <- gets false_monkey
  test_num <- gets test
  let dotest x = (if mod x test_num ==0 then tru else fals,x)
  put monke{items=[], inspections=inspections monke + length its}
  return $ map (dotest. (`div` 3) . op) its

monkeyTurn2 :: Int -> State Monkey [(Int,Int)]
monkeyTurn2 bcd = do
  monke <- get
  op <- gets operation
  its <- gets items
  tru <- gets true_monkey
  fals <- gets false_monkey
  test_num <- gets test
  let dotest x = (if mod x test_num ==0 then tru else fals,x)
  put monke{items=[], inspections=inspections monke + length its}
  return $ map (dotest . (`mod` bcd) . op) its

getMonkey :: Int -> State Monkeys Monkey
getMonkey n = do
  monkelist <- get
  return $ monkelist !! n

putMonkey :: Int -> Monkey -> State Monkeys ()
putMonkey n monke = do
  monkelist <- get
  put $ take n monkelist ++ (monke:drop (n+1) monkelist)

forMonkey :: Int -> State Monkey a -> State Monkeys a
forMonkey n action = do
  monke <- getMonkey n
  let (a,s) = runState action monke
  putMonkey n s
  return a

giveMonkeyItem :: Int -> State Monkey ()
giveMonkeyItem n = do
  monke <- get
  its <- gets items
  put monke{items=n:its}

fullMonkeyTurn1 :: Int -> State Monkeys ()
fullMonkeyTurn1 n = do
  its <- forMonkey n monkeyTurn1
  mapM_ (\(n,it)-> forMonkey n $ giveMonkeyItem it) its

fullMonkeyTurn2 :: Int -> Int -> State Monkeys ()
fullMonkeyTurn2 n bcd = do
  its <- forMonkey n $ monkeyTurn2 bcd
  mapM_ (\(n,it)-> forMonkey n $ giveMonkeyItem it) its

monkeyRound1 :: State Monkeys ()
monkeyRound1 = do
  num <- gets length
  mapM_ fullMonkeyTurn1 [0..num-1]

monkeyRound2 :: State Monkeys ()
monkeyRound2 = do
  num <- gets length
  moduli <- gets (map test)
  mapM_ (`fullMonkeyTurn2` product moduli ) [0..num-1]

main :: IO ()
main = interact ( show . product . drop (numMonkeys - 2) . sort . puzzle2 . map (parseMonkey . preParse) . splitMonkeys . lines)
-- main = interact id
numMonkeys = 8

puzzle1 :: Monkeys -> [Int]
puzzle1 = map inspections . execState (mapM_ (const monkeyRound1) [1..20])

puzzle2 :: Monkeys -> [Int]
puzzle2 = map inspections . execState (mapM_ (const monkeyRound2) [1..10000])

parseOperation :: String -> Int -> Int
parseOperation (c:' ':stuff) n = op n other where
  other = case stuff of 
    "old" -> n
    number -> read number
  op = case c of
    '+' -> (+)
    '*' -> (*)

preParse :: [String] -> [String]
preParse [a,b,c,d,e,f]= [drop 18 b, drop 23 c, drop 21 d, drop 29 e,drop 30 f]

removeTrailComma :: String -> String
removeTrailComma s | last s == ',' = removeTrailComma $ init s
                   | otherwise     = s


parseMonkey :: [String] -> Monkey
parseMonkey [itsString,opString,testString,truString,falString] = Monkey items operation test true_monkey false_monkey 0
  where
    items = map (read . removeTrailComma) $ words itsString
    operation = parseOperation opString
    test = read testString
    true_monkey = read truString
    false_monkey = read falString

splitMonkeys :: [String] -> [[String]]
splitMonkeys = splitMonkeys' []

splitMonkeys' :: [String] -> [String] -> [[String]]
splitMonkeys' linesBuffer [] = [linesBuffer]
splitMonkeys' linesBuffer ("":linesIn) = linesBuffer:splitMonkeys linesIn
splitMonkeys' linesBuffer (a:linesIn) = splitMonkeys' (linesBuffer ++ [a]) linesIn



-- Monkey 2:
--   Starting items: 85, 79, 59, 64, 79, 95, 67
--   Operation: new = old + 8
--   Test: divisible by 5
--     If true: throw to monkey 4
--     If false: throw to monkey 5
