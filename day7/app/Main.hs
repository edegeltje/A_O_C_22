module Main where

import Data.List
import Data.List.Split

import Control.Monad.State

-- splitOn :: [a] -> [a] -> [[a]]
-- splitOn = undefined

main :: IO ()
main = interact (show . puzzle2 . fst . execState parseOutput . makeStartState . splitOn "$ ")

makeStartState :: [String] -> (Dir,[String])
makeStartState a = (Dir "/" $ Folder Nothing, a)

tabify :: String -> String
tabify = unlines . map ("  " ++) . lines

size :: Dir -> Int
size (Dir _ c) = sizeC c

cutOff = 100000

puzzle1 :: Dir -> Int
puzzle1 dir = finalNumberC $ content dir

finalNumberC :: Content -> Int
finalNumberC (File n) = 0
finalNumberC (Folder Nothing) = 0
finalNumberC c@(Folder (Just ds)) = if sizeC c > cutOff
  then sum $ map puzzle1 ds
  else sizeC c + sum (map puzzle1 ds)

requiredSize = 30000000
maxSize = 70000000

puzzle2 :: Dir -> Int
puzzle2 dir = whatToDelete (requiredSize + size dir - maxSize) dir

whatToDelete :: Int -> Dir -> Int
whatToDelete cutoff (Dir _ c) = if whatToDeleteC cutoff c < cutoff
  then 0
  else whatToDeleteC cutoff c

whatToDeleteC :: Int -> Content -> Int
whatToDeleteC _ (File n) = 0
whatToDeleteC _ (Folder Nothing) = 0
whatToDeleteC cutoff c@(Folder (Just ds)) = if map (whatToDelete cutoff) ds == map (const 0) ds
  then sizeC c
  else minimum $ filter (/=0) $ map (whatToDelete cutoff) ds


sizeC :: Content -> Int
sizeC (File n) = n
sizeC (Folder Nothing) = undefined
sizeC (Folder (Just ds)) = sum $ map size ds

parseOutput :: State (Dir, [String]) ()
parseOutput = do
  mAction <- popAction
  case mAction of
    Nothing -> return ()
    Just a@"cd /\n" -> do
      dir <- getDir
      if name dir == "/" -- if we're at top level:
        then parseOutput -- continue
        else do
          addAction a -- put the action back, and break the loop at the current level
    Just "cd ..\n" -> return () -- break the loop at the current level
    Just ('c':'d':' ':r) -> do
      subDir (init r)
      parseOutput
    Just ('l':'s':'\n':r) -> do
      setSubDirs $ map parseSubDir $ lines r
      parseOutput
    _ -> parseOutput


parseSubDir :: String -> Dir
parseSubDir ('d':'i':'r':' ':r) = Dir r (Folder Nothing)
parseSubDir file = (\[a,b] -> Dir b $ File $ read a) $ splitOn " " file

setSubDirs :: [Dir] -> State (Dir,[String]) ()
setSubDirs sdirs = do
  dir <- getDir
  putDir dir{content=Folder (Just sdirs)}

subDir :: String -> State (Dir, [String]) ()
subDir dirname = do
  dir <- getDir
  case content dir of
    File f -> return ()
    Folder Nothing -> return ()
    Folder (Just as) -> do
      let subdirIndexm = findIndex ((dirname ==) . name) as
      case subdirIndexm of
        Nothing -> return ()
        Just subdirIndex -> do
          let (top,subdir:bottom) = splitAt subdirIndex as
          actions <- getActions
          let result = runState parseOutput (subdir,actions)
          setSubDirs (top ++ fst (snd result):bottom)
          putActions $ snd $ snd result

getDir :: State (Dir, [String]) Dir
getDir = do
  (dir, _) <- get
  return dir

putDir :: Dir -> State (Dir, [String]) ()
putDir dir = do
  (_,actions) <- get
  put (dir,actions)

getActions :: State (Dir,[String]) [String]
getActions = do
  (_,a) <- get
  return a

putActions :: [String] -> State (Dir,[String]) ()
putActions actions = do
  (dir,_) <- get
  put (dir,actions)

popAction :: State (Dir,[String]) (Maybe String)
popAction = do
  actions <- getActions
  case actions of
    [] -> return Nothing
    a:rest -> do
      putActions rest
      return (Just a)

addAction :: String -> State (Dir,[String]) ()
addAction action = do
  actions <- getActions
  putActions (action:actions)

data Content = Folder (Maybe [Dir]) | File Int
  deriving Eq

instance Show Content where
  show (File n) = "(file, size=" ++ show n ++ ")\n"
  show (Folder Nothing) = "(dir?)\n"
  show (Folder (Just fs)) = "(dir)\n" ++ concatMap (tabify . show) fs

data Dir = Dir {
  name :: String,
  content :: Content
}
  deriving Eq

instance Show Dir where
  show (Dir n c) = "- " ++ n ++ " " ++ show c



