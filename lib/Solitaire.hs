module Solitaire where

import Data.List.Index (imap)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Linear (V2(..))

data Direction
    = DUp
    | DDown
    | DLeft
    | DRight
    deriving (Show, Eq, Enum)

type Position a = V2 a

type Move a = (Position a, Direction)

type Board a = M.Map (Position a) Bool

directions :: [Direction]
directions = [DUp .. DRight]

dirToPos :: (Integral a) => Direction -> Position a
dirToPos DUp = V2 0 1
dirToPos DDown = V2 0 (-1)
dirToPos DLeft = V2 (-1) 0
dirToPos DRight = V2 1 0

flipPos :: (Integral a) => Position a -> Board a -> Board a
flipPos = M.adjust not

-- When only one coordinate is occupied the board is considered solved
isSolved :: (Integral a) => Board a -> Bool
isSolved = (1==) . M.foldr (\x acc -> if x then acc+1 else acc) 0

-- Checks that all 3 positions involved in a move are on the board
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
    usedSqrs =
        M.foldrWithKey
            (\k x acc ->
                 if x
                     then k : acc
                     else acc)
            []
            board
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

translateBoard :: (Integral a) => Position a -> Board a -> Board a
translateBoard delta = M.mapKeys (delta+)

printBoard :: (Integral a) => Board a -> IO ()
printBoard board =
    putStrLn $
    unlines
        [ [ convChar (M.lookup pos board)
        | x <- [minx .. maxx]
        , let pos = V2 x y
        ]
        | y <- [miny .. maxy]
        ]
  where
    convChar (Just True) = 'X'
    convChar (Just False) = '.'
    convChar Nothing = ' '
    maxx = foldl max (-1000) . map (\(V2 x _) -> x) . M.keys $ board
    maxy = foldl max (-1000) . map (\(V2 _ y) -> y) . M.keys $ board
    minx = foldl min 1000 . map (\(V2 x _) -> x) . M.keys $ board
    miny = foldl min 1000 . map (\(V2 _ y) -> y) . M.keys $ board

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
standardBoard = translateBoard (V2 (-3) (-3)) $ readBoard brd
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

-- Create a square board with sidelength n
mkSquare :: (Integral a) => a -> Board a
mkSquare n =
    flipPos (V2 (n `div` 2) (n `div` 2)) . readBoard $
    replicate (fromIntegral n) (replicate (fromIntegral n) 'X')

-- Create a triangular board with sidelength n
mkTriangle :: (Integral a) => a -> Board a
mkTriangle n =
    flipPos (V2 0 0) $
    readBoard [replicate (fromIntegral i) 'X' | i <- [n,n - 1 .. 1]]
