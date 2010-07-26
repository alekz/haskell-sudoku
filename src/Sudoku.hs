module Sudoku where

import Data.List ((\\), nub)

type Coord = Int  -- 1..9
type CellCoord = (Coord, Coord)
type Cell = Int  -- 0..9
type Board = [[Cell]]

cells :: [CellCoord]
cells = [(x,y) | x <- [1..9], y <- [1..9]]

row :: CellCoord -> [CellCoord]
row (x, y) = [(x', y) | x' <- ([1..9] \\ [x])]

col :: CellCoord -> [CellCoord]
col (x, y) = [(x, y') | y' <- ([1..9] \\ [y])]

group :: CellCoord -> [CellCoord]
group (x, y) = [(x', y') | x' <- groupCoords x, y' <- groupCoords y] \\ [(x, y)]
    where firstCoord n = 3 * ((n - 1) `div` 3) + 1
          groupCoords n = take 3 [(firstCoord n)..]

relatedCells :: CellCoord -> [CellCoord]
relatedCells coord = nub $ (row coord) ++ (col coord) ++ (group coord)

boardCell :: Board -> CellCoord -> Cell
boardCell board (x,y) = board !! (y - 1) !! (x - 1)

relatedCellsValues :: Board -> CellCoord -> [Cell]
relatedCellsValues board coord = nub [boardCell board c | c <- relatedCells coord] \\ [0]

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
