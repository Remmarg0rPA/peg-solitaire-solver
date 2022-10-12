module Main where

import Prelude hiding (Left, Right)
import qualified Data.Vector as V
import Control.Lens.Lens ((??))

data Used
  = Empty
  | Occupied
  deriving (Show, Eq)

data Direction
 = Up
 | Down
 | Left
 | Right
 deriving (Show, Eq, Enum)

type Position a
  = V.Vector a

type Board a
  = [(Position a, Used)]

instance Enum Used where
  toEnum 0 = Empty
  toEnum 1 = Occupied
  fromEnum Empty    = 0
  fromEnum Occupied = 1
  succ Empty    = Occupied
  succ Occupied = Empty
  pred = succ

directions :: [Direction]
directions = [Up, Down, Left, Right]

dirToPos :: (Integral a) => Direction -> Position a
dirToPos Up    = V.fromList [0,-1]
dirToPos Down  = V.fromList [0,1]
dirToPos Left  = V.fromList [-1,0]
dirToPos Right = V.fromList [1,0]

flipPos :: (Integral a) => Position a -> Board a -> Board a
flipPos pos board = [if pos /= p then (p, s) else (p, succ s) | (p,s)<-board]

makeMove :: (Integral a) => Position a -> Direction -> Board a -> Maybe (Board a)
makeMove pos dir board =
  if not (validMove pos dir board)
    then Nothing
    else Just newBoard
  where
    newBoard = foldl1 (.) (map flipPos [pos, V.zipWith (+) pos (dirToPos dir), V.zipWith (+) pos (fmap (2*) (dirToPos dir))]) board


inBounds :: (Integral a) => Position a -> Bool
inBounds = or . (??) [p1, p2, p3]
  where
    p1 v = (v V.! 0) `elem` [0..1] && (v V.! 1) `elem` [2..4]
    p2 v = (v V.! 0) `elem` [2..4] && (v V.! 1) `elem` [0..6]
    p3 v = (v V.! 0) `elem` [5..6] && (v V.! 1) `elem` [2..4]

validMove :: (Integral a) => Position a -> Direction -> Board a -> Bool
validMove pos dir board = inBoundsP && fromP && overP && toP
  where
    fromPos = pos
    overPos = V.zipWith (+) pos (dirToPos dir)
    toPos   = V.zipWith (+) pos ((2*) <$> dirToPos dir)
    inBoundsP = all inBounds [fromPos, overPos, toPos]
    fromP = generalP fromPos Occupied
    overP = generalP overPos Occupied
    toP   = generalP toPos    Empty
    generalP pos s = case lookup pos board of
                        Nothing -> False
                        Just p  -> p == s

board :: (Integral a) => Board a
board = [(V.fromList [x,y], if x==3 && y==3 then Empty else Occupied) | x<-[0..6], y<-[0..6], inBounds (V.fromList [x,y])]

printBoard :: (Integral a) => Board a -> IO()
printBoard board = putStrLn $ unlines [[convChar (lookup pos board) | x<-[0..(fromIntegral width)], let pos = V.fromList [x,y]] | y<-[0..height]]
  where
    convChar (Just Occupied) = 'X'
    convChar (Just Empty)    = '.'
    convChar Nothing       = ' '
    width  = foldl max (-1) $ map ((V.!0) . fst) board
    height = foldl max (-1) $ map ((V.!1) . fst) board


main :: IO()
main = do
  print board
