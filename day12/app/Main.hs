{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = interact $ puzzle2 . newSolution . lines

showPath :: Solution -> String
showPath s = unlines $ fromPath $ snd $ execState (do 
  createPath st end
  ) s where
  st = findCharCoords 'S' $ fst s
  end = findCharCoords 'E' $ fst s

puzzle1 :: Solution -> String
puzzle1 s = show $ evalState (do 
  createPath st end
  forPath $ pathLength $ fromJust st
  ) s where
  st = findCharCoords 'S' $ fst s
  end = findCharCoords 'E' $ fst s

puzzle2 :: Solution -> String
puzzle2 s = show $ evalState (do
  createPath st end
  Map m <- getMap
  let startingPoints = map (\(y,x) -> (x,y)) $ catMaybes $ zipWith (curry expandMaybeTuple) [0 .. ] $ map (elemIndex 'a') m
  let exStartings = fromJust st:startingPoints
  lengths <- mapM (forPath . pathLength) exStartings
  return $ minimum $ catMaybes lengths
  ) s where
    st = findCharCoords 'S' $ fst s
    end = findCharCoords 'E' $ fst s



newtype Map = Map [[Char]] deriving Show

fromMap :: Map -> [String]
fromMap (Map m) = m

newtype Path = Path [[Char]] deriving Show

fromPath :: Path -> [String]
fromPath (Path p) = p

type Coords = (Int,Int)

type Solution = (Map,Path)

type SolveState = State Solution

newSolution :: [String] -> (Map,Path)
newSolution m = (Map m,Path $ map (map $ const '.') m)

pathLength :: Coords -> State Path (Maybe Int)
pathLength (x,y) = do
  mc <- getCoordsPath (x,y)
  case mc of
    Nothing -> return Nothing
    Just c -> do
      case c of
        'E' -> return $ Just 0
        '^' -> fmap (+1) <$> pathLength (x, y-1)
        '>' -> fmap (+1) <$> pathLength (x+1,y)
        'V' -> fmap (+1) <$> pathLength (x, y+1)
        '<' -> fmap (+1) <$> pathLength (x-1,y)
        _ -> return Nothing 

getMap :: SolveState Map
getMap = do
  gets fst

getPath :: SolveState Path
getPath = do
  gets snd

setMap :: Map -> SolveState ()
setMap m = do
  p <- getPath
  put (m,p)

setPath :: Path -> SolveState ()
setPath p = do
  m <- getMap
  put (m,p)

forPath :: State Path a -> SolveState a
forPath action = do
  p <- getPath
  let (a,s) = runState action p
  setPath s
  return a

forMap :: State Map a -> SolveState a
forMap action = do
  m <- getMap
  let (a,s) = runState action m
  setMap s
  return a

atIndex :: [a] -> Int -> Maybe a
atIndex [] _ = Nothing
atIndex (a:as) 0 = Just a
atIndex (a:as) x = atIndex as (x-1)

getCoordsPath :: Coords -> State Path (Maybe Char)
getCoordsPath (x,y) =do 
  Path p <- get
  return $ do 
    row <- atIndex p y
    atIndex row x

getCoordsMap :: Coords -> State Map (Maybe Char)
getCoordsMap (x,y) = do 
  Map m <- get
  return $ do 
    row <- atIndex m y
    atIndex row x

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = [] 
replace 0 b (a:as) = b:as
replace x b (a:as) = a:replace (x-1) b as

setCoordsPath :: Coords -> Char -> State Path ()
setCoordsPath (x,y) c = do
  cr <- getCoordsPath (x,y)
  case cr of
    Just '.' -> do
      Path p <- get
      let col = p !! y
      let newcol = replace x c col
      put $ Path $ replace y newcol p
    _ -> return ()

neighbours :: Coords -> [Coords]
neighbours (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

neighboursWithVals :: Coords -> State Map [Maybe Char]
neighboursWithVals c = mapM getCoordsMap $ neighbours c

compareHeights :: Char -> Char -> Bool
compareHeights 'S' to = compareHeights 'a' to
compareHeights 'E' to = compareHeights 'z' to
compareHeights from 'S' = compareHeights from 'a'
compareHeights from 'E' = compareHeights from 'z'
compareHeights from to = ord from >= ord to -1

compareWithNeighbours :: Coords -> State Map [Bool]
compareWithNeighbours c = do
  cval <- getCoordsMap c
  case cval of
    Nothing -> return [False, False, False, False]
    Just cval -> do
      nvals <- neighboursWithVals c
      return $ map (maybe False (`compareHeights` cval)) nvals

setUnsetNeighbours :: Coords -> SolveState ()
setUnsetNeighbours c = do
  pathChar <- forPath $ getCoordsPath c
  case pathChar of
    Nothing -> return ()
    Just '.' -> return ()
    Just _ -> do
      ncompares <- forMap $ compareWithNeighbours c
      let stuff = zip (neighbours c) $ zip "<^>V" ncompares
      mapM_ (\(co, (cr,b)) -> do
        when b $ forPath $ setCoordsPath co cr
        ) stuff

expandMaybeTuple :: (a,Maybe b) -> Maybe (a,b)
expandMaybeTuple (_,Nothing) = Nothing
expandMaybeTuple (a, Just b) = Just (a,b)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:ms) = firstJust ms
firstJust (a:_) = a

findCharCoords :: Char -> Map -> Maybe Coords
findCharCoords c (Map m) = fmap (\(y,x) -> (x,y)) $ firstJust $ zipWith (curry expandMaybeTuple) [0 .. ] $ map (elemIndex c) m


iterateAllPath :: SolveState ()
iterateAllPath = do
  Map m <- getMap
  let allCoords = concat $ zipWith (\a -> map (a,)) [0..] $ map (zipWith const [0..]) m
  mapM_ setUnsetNeighbours allCoords



createPath :: Maybe Coords -> Maybe Coords ->  SolveState ()
createPath Nothing _ = return ()
createPath _ Nothing = return ()
createPath (Just start) (Just end) = do
  forPath $ setCoordsPath end 'E'
  Map m <- getMap
  let allCoords = concat $ zipWith (\a -> map (,a)) [0..] $ map (zipWith const [0..]) m

  mapM_ setUnsetNeighbours allCoords

  c <- forPath $ getCoordsPath start
  case c of
    Nothing -> return ()
    Just '.' -> createPath (Just start) $ Just end
    _ -> return ()