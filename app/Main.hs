module Main where

import Prelude hiding (Left, Right)
import Data.Maybe (fromJust)
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

type Move a
  = (Position a, Direction)

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
dirToPos Up    = tupPos (0, -1)
dirToPos Down  = tupPos (0, 1)
dirToPos Left  = tupPos (-1, 0)
dirToPos Right = tupPos (1, 0)

flipPos :: (Integral a) => Position a -> Board a -> Board a
flipPos pos board = [if pos /= p then (p, s) else (p, succ s) | (p,s)<-board]

tupPos :: (Integral a) => (a, a) -> Position a
tupPos (x, y) = V.fromList [x, y]

makeMove :: (Integral a) => Move a -> Board a -> Maybe (Board a)
makeMove (pos, dir) board =
  if not (validMove (pos, dir) board)
    then Nothing
    else Just $ updateBoard board
  where
    updateBoard = foldl1 (.) (map flipPos [pos,
                                        V.zipWith (+) pos (dirToPos dir),
                                        V.zipWith (+) pos (fmap (2*) (dirToPos dir))])

isSolved :: (Integral a) => Board a -> Bool
isSolved = (1==) . length . filter ((==Occupied) . snd)

validMove :: (Integral a) => Move a -> Board a -> Bool
validMove (pos, dir) board = all (`elem` board) [(fromPos, Occupied),
                                                 (overPos, Occupied),
                                                 (toPos, Empty)]
  where
    fromPos = pos
    overPos = V.zipWith (+) pos (dirToPos dir)
    toPos   = V.zipWith (+) pos ((2*) <$> dirToPos dir)

printBoard :: (Integral a) => Board a -> IO()
printBoard board =
  putStrLn $ unlines [[convChar (lookup pos board)
                      | x<-[0..(fromIntegral width)], let pos = tupPos (x,y)]
                     | y<-[0..height]]
  where
    convChar (Just Occupied) = 'X'
    convChar (Just Empty)    = '.'
    convChar Nothing       = ' '
    width  = foldl max (-1) $ map ((V.!0) . fst) board
    height = foldl max (-1) $ map ((V.!1) . fst) board

bruteForceSolution :: (Integral a) => Board a -> Maybe [Move a]
bruteForceSolution board
  | isSolved board = Just []
  | null allValidMoves = Nothing
  | otherwise =
      let brds = filter ((/=Nothing) . fst) [(bruteForceSolution (fromJust $ makeMove mv board), mv) | mv<-allValidMoves] in
        case brds of
          [] -> Nothing
          (Just mvs, mv):_ -> Just (mv:mvs)
  where
    occupiedSquares = map fst $ filter ((==Occupied) . snd) board
    allValidMoves = filter (`validMove` board) [(pos, dir) | pos<-occupiedSquares, dir<-directions]

-- The standard board
board :: (Integral a) => Board a
board = [(tupPos (x,y), if x==3 && y==3 then Empty else Occupied) | x<-[0..6], y<-[0..6], inBounds (tupPos (x,y))]

-- Specific to the standard board
inBounds :: (Integral a) => Position a -> Bool
inBounds = or . (??) [p [2..4] [0..1],
                      p [0..6] [2..4],
                      p [2..4] [5..6]]
  where
    p xR yR v = (v V.! 0) `elem` xR && (v V.! 1) `elem` yR

main :: IO()
main = do
  print $ bruteForceSolution board
