module Main where

import Data.List (sortOn)
import Data.List.Index (imap)
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

type Board a = [(Position a, Bool)]

directions :: [Direction]
directions = [Up, Down, Left, Right]

dirToPos :: (Integral a) => Direction -> Position a
dirToPos Up = V2 0 (-1)
dirToPos Down = V2 0 1
dirToPos Left = V2 (-1) 0
dirToPos Right = V2 1 0

flipPos :: (Integral a) => Position a -> Board a -> Board a
flipPos pos board =
    [ if pos /= p
        then (p, s)
        else (p, not s)
    | (p, s) <- board
    ]

-- When only one point is occupied the board is considered solved
isSolved :: (Integral a) => Board a -> Bool
isSolved = (1 ==) . length . filter snd

-- Checks that all 3 points involved in a move is on the board
isValidMove :: (Integral a) => Move a -> Board a -> Bool
isValidMove (pos, dir) board =
    all (`elem` board) [(pos, True), (pos + d, True), (pos + 2 * d, False)]
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

-- Sorts the board based on the x positions
sortBoard :: (Integral a) => Board a -> Board a
sortBoard = sortOn ((\(V2 x _) -> x) . fst)

printBoard :: (Integral a) => Board a -> IO ()
printBoard board =
    putStrLn $
    unlines
        [ [convChar (lookup pos board) | x <- [0 .. width], let pos = V2 x y]
        | y <- [0 .. height]
        ]
  where
    convChar (Just True) = 'X'
    convChar (Just False) = '.'
    convChar Nothing = ' '
    width = foldl max (-1) $ map ((\(V2 x _) -> x) . fst) board
    height = foldl max (-1) $ map ((\(V2 _ y) -> y) . fst) board

-- Converts a list of strings (rows of the board) into a board
readBoard :: (Integral a) => [String] -> Board a
readBoard = concat . imap genPos
  where
    genPos i rw =
        [ (V2 (fromIntegral x) (fromIntegral i), rw !! x == 'X')
        | x <- [0 .. length rw - 1]
        , rw !! x /= ' '
        ]

-- Iterates through all directions for all occupied points on the board
bruteForceSolution :: (Integral a) => Board a -> Maybe [Move a]
bruteForceSolution board
    | isSolved board = Just []
    | null allValidMoves = Nothing
    | otherwise =
        case solution of
            (sol, mv):_ -> (mv :) <$> sol -- prepend last move to solution
            _ -> Nothing
  where
    usedSqrs = map fst . filter snd $ board -- Coordinates for occupied squares
    allValidMoves =
        filter
            (`isValidMove` board)
            [(pos, dir) | pos <- usedSqrs, dir <- directions]
    solution =
        filter
            (not . null . fst)
            [ (bruteForceSolution (fromJust $ makeMove mv board), mv)
            | mv <- allValidMoves
            ]

-- The board is sorted to make finding a solution quicker
standardBoard :: (Integral a) => Board a
standardBoard = sortBoard $ readBoard brd
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
