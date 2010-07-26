module Sudoku where

import Data.List ((\\), nub)

cells = [(x,y) | x <- [1..9], y <- [1..9]]

row (x, y) = [(x', y) | x' <- ([1..9] \\ [x])]

col (x, y) = [(x, y') | y' <- ([1..9] \\ [y])]

group (x, y) = [(x', y') | x' <- groupCoords x, y' <- groupCoords y] \\ [(x, y)]
    where firstCoord n = 3 * ((n - 1) `div` 3) + 1
          groupCoords n = take 3 [(firstCoord n)..]

relatedCells cell = nub $ (row cell) ++ (col cell) ++ (group cell)

boardCell board (x,y) = board !! (y - 1) !! (x - 1)

relatedCellsValues board cell = nub [boardCell board c | c <- relatedCells cell] \\ [0]

{-
replaceCell board cell value = ...

solveCell board cell
    | isSolved    = board
    | hasSolution = replaceCell board cell cellSolution
    | otherwise   = board
    where isSolved = boardCell board cell /= 0
          hasSolution = length relatedCellsValues == 8
          cellSolution = head $ [1..9] \\ relatedCellsValues
-}

-- solve board = foldl solveCell board cells
