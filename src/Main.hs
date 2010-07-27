module Main where

import Data.Maybe (fromJust, isNothing)
import qualified Sudoku

main::IO()
main = do
    contents <- getContents
    let maybeBoard = Sudoku.fromString contents
    if isNothing maybeBoard
        then putStrLn "Invalid sudoku data"
        else do
            let board = fromJust maybeBoard
            putStrLn "== Unsolved ==\n"
            putStrLn $ Sudoku.printBoard board
            putStrLn "== Solved ==\n"
            putStrLn $ Sudoku.printBoard $ Sudoku.solve board
