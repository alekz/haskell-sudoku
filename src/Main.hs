module Main where

import Data.Maybe (fromJust, isNothing)
import qualified Sudoku

main::IO()
main = do interact result

result contents
    | isNothing maybeBoard = "Invalid sudoku data"
    | otherwise = unlines ["Original", p board, "Solved", p solvedBoard]
    where maybeBoard = Sudoku.fromString contents
          board = fromJust maybeBoard
          solvedBoard = Sudoku.solve board
          p = Sudoku.printBoard
