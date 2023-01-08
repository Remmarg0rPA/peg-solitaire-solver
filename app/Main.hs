module Main where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List (nub)
import GHC.Utils.Misc (split)
import Solitaire
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)

data Options =
    Options
        { optVerbose :: Bool -- Print the board which is being solved?
        , optStandard :: Bool -- Include standard board
        , optTriangle :: [Int] -- side lengths of triangular boards
        , optSquare :: [Int] -- side length of square boards
        }
    deriving (Show, Eq)

startOptions :: Options
startOptions =
    Options
        { optVerbose = True
        , optStandard = False
        , optTriangle = []
        , optSquare = []
        }

-- Parses "1,2,3" into [1, 2, 3] and support ranges, "2-4" -> [2,3,4],
-- which can be included in the list, "1,2,5-10" -> [1,2,5,6,7,8,9,10].
-- The step size in ranges can be specified with eg "1:2..5" -> [1,3,5].
parseListRange :: String -> [Int]
parseListRange = concatMap parseRange . split ','
  where
    parseRange :: String -> [Int]
    parseRange = splitToList . split '-'
    splitToList :: [String] -> [Int]
    splitToList [min, max] =
        case split ':' min of
            [min', step] -> [(read min'),(read step + read min') .. (read max)]
            [min'] -> [read min' .. read max]
            _ -> error "Incorrect use of ranges"
    splitToList x = map read x

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option
          ['q']
          ["quiet"]
          (NoArg (\opt -> return opt {optVerbose = False}))
          "Do not print the board which is being solved"
    , Option
          "c"
          ["standard-board"]
          (NoArg (\opt -> return opt {optStandard = True}))
          "Solve the standard board"
    , Option
          "t"
          ["triangle"]
          (ReqArg
               (\arg opt -> return opt {optTriangle = parseListRange arg})
               "Side lengths")
          "Create triangle boards with the specified side lengths"
    , Option
          "s"
          ["square"]
          (ReqArg
               (\arg opt -> return opt {optSquare = parseListRange arg})
               "Side lengths")
          "Create square board with the specified side lengths"
    , Option "h?" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitSuccess))
        "Show help"
    ]

solve :: (Integral a, Show a) => [Board a] -> Bool -> IO ()
solve boards verbose = traverse_ printer boards
  where
    printer board =
        when verbose (printBoard board) >>
        print (bruteForceSolution board) >>
        when verbose (putStrLn "------------\n")

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt Permute options args
    opts <- foldl (>>=) (return startOptions) actions
    let boards =
            nub $ -- Filter out duplicates
            map mkTriangle (optTriangle opts) ++
            map mkSquare (optSquare opts) ++ [standardBoard | optStandard opts]
    solve boards (optVerbose opts)
    return ()
