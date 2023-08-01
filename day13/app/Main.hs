module Main where
import Data.Bifunctor
import Data.List

data IntTree = Root [IntTree] | Node Int deriving (Show)

instance Eq IntTree where
  (==) (Node left) (Node right) = left == right
  (==) (Root left) (Node right) = Root left == Root [Node right]
  (==) (Node left) (Root right) = Root [Node left] == Root right
  (==) (Root a) (Root b) = a == b


instance Ord IntTree where
  -- if they are both ints, compare them
  (<=) (Node left) (Node right) = left <= right
  -- if one is an int, make it a list
  (<=) (Root left) (Node right) = Root left <= Root [Node right]
  -- if the other is an int, make it a list
  (<=) (Node left) (Root right) = Root [Node left] <= Root right
  -- if the left one is empty first, we're ok
  (<=) (Root []) _ = True
  -- if the right one is empty first, we're not ok
  (<=) _ (Root []) = False
  -- iterate over first elements to do the rest
  (<=) (Root (a:left)) (Root (b:right)) = a < b || (a == b && Root left <= Root right)

main :: IO ()
main = interact $ show . puzzle2 . lines

puzzle1 :: [String] -> Int
puzzle1 = sum . getOrderedIndices . isOrdered . splitPairs

separatorPackets :: [IntTree]
separatorPackets = [Root [Root [Node 2]], Root [Root [Node 6]]]

isSeparatorPacket :: IntTree -> Bool
isSeparatorPacket x = x == Root [Root [Node 2]] || x == Root [Root [Node 6]]

puzzle2 :: [String] -> Int
puzzle2 = product . getOrderedIndices .  map isSeparatorPacket . sort . (++ separatorPackets ) . map parseTree . removeEmpties

getOrderedIndices :: [Bool] -> [Int]
getOrderedIndices = map fst . filter snd . zip [1..]

isOrdered :: [(String,String)] -> [Bool]
isOrdered = map (uncurry (<=) . bimap parseTree parseTree)

removeEmpties :: [String] -> [String]
removeEmpties [] = []
removeEmpties ("":strs) = removeEmpties strs
removeEmpties (a:strs) = a:removeEmpties strs

splitPairs :: [String] -> [(String,String)]
splitPairs [] = []
splitPairs [a] = [(a,"")]
splitPairs [a,b] = [(a,b)]
splitPairs ("":something) = splitPairs something
splitPairs (a:b:something) = (a,b):splitPairs something

parseTree :: String -> IntTree

parseTree [] = Root []
parseTree ('[':rest) = Root $ map parseTree $ (`split` "") $ init rest
parseTree number = Node $ read number


-- parseTree "[]" = Root $ map parseTree $ split "" ""
-- split "" "" = []
-- parseTree "[]" = Root $ map parseTree []
-- parseTree "[]" = Root []


split :: String -> String -> [String]
split [] [] = []
split [] buffer = [buffer]
split (',':rest) buffer = buffer:split rest []
split ('[':rest) buffer = keep rest (buffer ++ "[") 1
split (c:rest) buffer = split rest $ buffer ++ [c]

keep :: String -> String -> Int -> [String]
keep [] [] _ = []
keep [] buffer _ = [buffer]
keep rest buffer 0 = split rest buffer
keep ('[':rest) buffer w = keep rest (buffer ++ "[") (w+1)
keep (']':rest) buffer w = keep rest (buffer ++ "]") (w-1)
keep (c:rest) buffer w = keep rest (buffer ++ [c]) w
