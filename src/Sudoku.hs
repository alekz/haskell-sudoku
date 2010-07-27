module Sudoku where

-- Imports ---------------------------------------------------------------------

import qualified Data.Map as Map
import Data.Map ((!))
import Data.List ((\\), nub, intercalate)

-- Data types ------------------------------------------------------------------

type Coord = Int                 -- 1..9
type CellCoord = (Coord, Coord)  -- (x, y)
type CellValue = Int             -- 0..9; 0 = unknown value
type Board = Map.Map CellCoord CellValue

-- Utility functions -----------------------------------------------------------

-- Converts list to a Board type
fromList :: [[CellValue]] -> Board
fromList list = Map.fromList [((x, y), list !! (y - 1) !! (x - 1)) | x <- [1..9], y <- [1..9]]

-- Prints Sudoku board in a nice human-readable format
printBoard :: Board -> String
printBoard board =
    unlines $ intercalate [line] $ map printRows coordGroups
    where coordGroups = [[], [1..3], [4..6], [7..9], []]
          line = " +-------+-------+-------+"
          printRows ys = map printRow ys
          printRow y = intercalate " | " $ map (printCells y) coordGroups
          printCells y xs = intercalate " " $ map (printCell y) xs
          printCell y x = if value == 0 then "." else show value
              where value = board ! (x,y)

-- Solving functions -----------------------------------------------------------

-- Returns list of all Sudoku coordinates
coords :: [CellCoord]
coords = [(x, y) | x <- [1..9], y <- [1..9]]

-- For a given cell coordinate, returns related cells, i.e. all cells from
-- the same row, column and group
relatedCells :: CellCoord -> [CellCoord]
relatedCells (x, y) = nub $ rowCells ++ colCells ++ groupCells
    where rowCells = [(x', y) | x' <- ([1..9] \\ [x])]
          colCells = [(x, y') | y' <- ([1..9] \\ [y])]
          groupCells = [(x', y') | x' <- groupCoords x, y' <- groupCoords y] \\ [(x, y)]
              where firstCoord n = 3 * ((n - 1) `div` 3) + 1
                    groupCoords n = take 3 [(firstCoord n)..]

-- For a given cell coordinate, returns all values used in related cells,
-- i.e. in cells from the same row, column and group
relatedCellsValues :: Board -> CellCoord -> [CellValue]
relatedCellsValues board coord = nub [board ! c | c <- relatedCells coord] \\ [0]

-- Tries to find value for a single cell
solveCell :: Board -> CellCoord -> Board
solveCell board coord
    | isSolved    = board
    | hasSolution = Map.insert coord cellSolution board
    | otherwise   = board
    where isSolved = (board ! coord) /= 0
          allowedValues = [1..9] \\ (relatedCellsValues board coord)
          hasSolution = length allowedValues == 1
          cellSolution = head allowedValues

-- Solves the Sudoku
solve :: Board -> Board
solve board
    | board == board' = board
    | otherwise       = solve board'
    where board' = foldl solveCell board coords
