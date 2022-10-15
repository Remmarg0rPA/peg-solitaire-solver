module Main where

import Data.List.Index (imap)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Linear (V2(..))
import Prelude hiding (Left, Right)

data Direction
    = Up
    | Down
    | Left
    | Right
    deriving (Show, Eq, Enum)

type Position a = V2 a

type Move a = (Position a, Direction)

type Board a = M.Map (Position a) Bool

directions :: [Direction]
directions = [Up .. Right]

dirToPos :: (Integral a) => Direction -> Position a
dirToPos Up = V2 0 (-1)
dirToPos Down = V2 0 1
dirToPos Left = V2 (-1) 0
dirToPos Right = V2 1 0

flipPos :: (Integral a) => Position a -> Board a -> Board a
flipPos = M.adjust not

-- When only one point is occupied the board is considered solved
isSolved :: (Integral a) => Board a -> Bool
isSolved = (1==) . M.foldr (\x acc -> if x then acc+1 else acc) 0

-- Checks that all 3 positions involved in a move is on the board
isValidMove :: (Integral a) => Move a -> Board a -> Bool
isValidMove (pos, dir) board =
    M.findWithDefault False pos board &&
    M.findWithDefault False (pos + d) board &&
    not (M.findWithDefault True (pos + 2 * d) board)
  where
    d = dirToPos dir

makeMove :: (Integral a) => Move a -> Board a -> Maybe (Board a)
makeMove (pos, dir) board =
    if not (isValidMove (pos, dir) board)
        then Nothing
        else Just $ updateBoard board
  where
    d = dirToPos dir
    updateBoard = foldl1 (.) (map flipPos [pos, pos + d, pos + 2 * d])

-- Iterates through all directions for all occupied points on the board
-- and returns the first solution found
bruteForceSolution :: (Integral a) => Board a -> Maybe [Move a]
bruteForceSolution board
    | isSolved board = Just []
    | null allValidMoves = Nothing
    | otherwise =
        case solution of
            (sol, mv):_ -> (mv :) <$> sol -- prepend last move to solution
            _ -> Nothing
  where
    -- Coordinates of occupied positions
    usedSqrs = map fst . M.toList . M.filter id $ board
    -- Filters out the valid moves from all of the possible combinations
    allValidMoves =
        filter
            (`isValidMove` board)
            [(pos, dir) | pos <- usedSqrs, dir <- directions]
    -- Picks the first solution found
    solution =
        filter
            (not . null . fst)
            [ (bruteForceSolution (fromJust $ makeMove mv board), mv)
            | mv <- allValidMoves
            ]

printBoard :: (Integral a) => Board a -> IO ()
printBoard board =
    putStrLn $
    unlines
        [ [convChar (M.lookup pos board) | x <- [0 .. width], let pos = V2 x y]
        | y <- [0 .. height]
        ]
  where
    convChar (Just True) = 'X'
    convChar (Just False) = '.'
    convChar Nothing = ' '
    width = foldl max (-1) . map (\(V2 x _) -> x) . M.keys $ board
    height = foldl max (-1) . map (\(V2 _ y) -> y) . M.keys $ board

-- Converts a list of strings (rows of the board) into a board
readBoard :: (Integral a) => [String] -> Board a
readBoard = M.fromList . concat . imap genPos
  where
    genPos i rw =
        [ (V2 (fromIntegral x) (fromIntegral i), rw !! x == 'X')
        | x <- [0 .. length rw - 1]
        , rw !! x /= ' '
        ]

standardBoard :: (Integral a) => Board a
standardBoard = readBoard brd
  where
    brd =
        [ "  XXX  "
        , "  XXX  "
        , "XXXXXXX"
        , "XXX.XXX"
        , "XXXXXXX"
        , "  XXX  "
        , "  XXX  "
        ]

main :: IO ()
main = do
    let board = standardBoard
    printBoard board
    print $ bruteForceSolution board
