module Main where

import System.Environment
import Data.Maybe (fromJust, isNothing)
import qualified Sudoku

main :: IO ()
main = do
    args <- getArgs
    if null args
        then processStdin
        else if length args == 1
            then processFile $ head args
            else printUsage

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    print $ "Usage: " ++ progName ++ " <filename>"

processStdin :: IO ()
processStdin = interact result

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    putStrLn $ result content

result :: String -> String
result contents
    | isNothing maybeBoard = "Invalid sudoku data"
    | otherwise = unlines ["Original", p board, "Solved", p solvedBoard]
    where maybeBoard = Sudoku.fromString contents
          board = fromJust maybeBoard
          solvedBoard = Sudoku.solve board
          p = Sudoku.printBoard
