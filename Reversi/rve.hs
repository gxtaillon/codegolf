{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding ((>>=),concatMap,filter,foldl,map,null,replicate)
import Control.Applicative hiding (empty)
import Control.Monad
import Data.Either
import Data.Vector hiding ((++),concat,foldM,reverse,zip)

type Case = Maybe Bool
type Board = Vector (Vector Case)
type Coord = (Int,Int)

-- fmap (toList) $ replicateM 2 [-1,0,1] -- minus (0,0)
directions :: Vector Coord
directions = fromList [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

--  [(x,y) | x <- [0..7], y <- [0..7]]
allCoords :: Vector Coord
allCoords = fromList [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]


initBoard :: Board
initBoard = vs // [(3,v1),(4,v2)]
    where va = replicate 8 Nothing
          v1 = va // [(3,Just True),(4,Just False)]
          v2 = va // [(3,Just False),(4,Just True)]
          vs = replicate 8 va

showBoard :: Board -> String
showBoard b = (concat $ toList $ map (showRow) b) ++ (showScore b)
    where showCase :: Case -> String
          showCase c = case c of 
              Just False -> "░B"
              Just True -> "▓W"
              Nothing -> "██"
          showRow :: Vector Case -> String
          showRow r = concat (toList $ map (showCase) r) ++ "\n"

showScore :: Board -> String
showScore b = winner score ++ "\n"
    where scoreCase :: (Int,Int) -> (Maybe Bool) -> (Int,Int)
          scoreCase (w,b) (Just True) = (w+1,b)
          scoreCase (w,b) (Just False) = (w,b+1)
          scoreCase s _ = s
          scoreRow :: Vector Case -> (Int,Int)
          scoreRow r = foldl (scoreCase) (0,0) r
          score :: (Int,Int)
          score = foldl (\(w,b) (rw,rb) -> (w+rw,b+rb)) (0,0) $ map (scoreRow) b
          winner :: (Int,Int) -> String
          winner (w,b) | w > b = "White with " ++ show w ++ " against Black with " ++ show b ++ "."
                       | b > w = "Black with " ++ show b ++ " against White with " ++ show w ++ "."
                       | otherwise = "Nobody with " ++ show (w+b) ++ "."

printBoard :: Board -> IO ()
printBoard b = putStrLn $ showBoard b
pm :: Either String Board -> IO ()
pm (Right b) = printBoard b
pm (Left s)  = putStrLn s

lookupBoard :: Board -> Coord -> Either String Case
lookupBoard b (x,y) = case b !? y of
    Just col -> case col !? x of
        Just c -> Right c
        Nothing -> Left "x is out of bounds"
    Nothing -> Left "y is out of bounds"

updateBoard :: Board -> Coord -> Bool -> Either String Board
updateBoard b (x,y) c = case b !? y of
    Just r -> case r !? x of
        Just _ -> Right $ b // [(y,r // [(x,Just c)])] 
        Nothing -> Left "x is out of bounds"
    Nothing -> Left "y is out of bounds"

makePath :: Board -> Coord -> Bool -> Coord -> Vector Coord
makePath b (x,y) c (px,py) = case lookupBoard b (nx,ny) of
        Right (Just pc) -> if pc == c
            then empty
            else case lookupBoard b (nx+px,ny+py) of
                Right (Just _) -> (nx,ny) `cons` makePath b (nx,ny) c (px,py)
                _ -> empty
        Right (Nothing) -> empty
        Left _ -> empty
    where nx = x+px
          ny = y+py

makeMove :: Board -> Coord -> Bool -> Either String Board
makeMove b xy@(x,y) c = if null cases 
        then Left $ "impossible move " ++ show xy ++ "."
        else foldM (\ob (cx,cy) -> updateBoard ob (cx,cy) c) b $ toList $ cases `snoc` (x,y)
    where cases = join $ map (makePath b (x,y) c) directions

makeMoves :: Board -> Vector (Bool,Coord) -> Either String Board
makeMoves b ms = foldM (\ob (c,xy) -> case makeMove ob xy c of
    rb@(Right _) -> rb
    Left _ -> if not $ null $ findIndices (\xy -> isRight $ makeMove ob xy c) allCoords
        then Left $ "wrong move " ++ show xy ++ "."
        else makeMove ob xy (not c)) b $ toList ms


movesFromString :: String -> Either String (Vector (Bool,Coord))
movesFromString cs = fromList <$> (zip $ cycle [False,True]) <$> coords cs
    where coords (x:y:cs) = (:) (read [x],read [y]) <$> coords cs
          coords (_:cs) = Left "invalid coordinates string"
          coords [] = Right []

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

main=getLine>>= \s->pm$movesFromString s>>=makeMoves initBoard
