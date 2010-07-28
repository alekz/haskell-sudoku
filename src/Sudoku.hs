module Sudoku
( fromString
, printBoard
, solve
) where

-- Imports ---------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord as Ord

import Data.Maybe (fromJust, isNothing)
import Data.Map ((!))
import Data.List ((\\))

-- Data types ------------------------------------------------------------------

type Coord = Int                 -- 1..9
type CellCoord = (Coord, Coord)  -- (x, y)
type CellValue = Int             -- 0..9; 0 = unknown value
type Board = Map.Map CellCoord CellValue

-- Solving functions -----------------------------------------------------------

-- For a given cell coordinate, returns related cells, i.e. all cells from
-- the same row, column and group
getRelatedCells :: CellCoord -> [CellCoord]
getRelatedCells (x, y) = rowCells ++ colCells ++ groupCells
    where rowCells = [(x', y) | x' <- ([1..9] \\ [x])]
          colCells = [(x, y') | y' <- ([1..9] \\ [y])]
          groupCells = [(x', y') | x' <- groupCoords x, y' <- groupCoords y]
              where firstCoord n = 3 * ((n - 1) `div` 3) + 1
                    groupCoords n = take 3 [(firstCoord n)..] \\ [n]

-- For a given cell coordinate, returns all values allowed for that cell
getAllowedCellValues :: Board -> CellCoord -> [CellValue]
getAllowedCellValues board coord = [1..9] \\ [board ! c | c <- getRelatedCells coord]

-- For a given board, returns coordinates of all empty cells
getEmptyCells :: Board -> [CellCoord]
getEmptyCells board = Map.keys $ Map.filter (== 0) board

-- Solves sudoku puzzle
solve :: Board -> Maybe Board
solve board
    | isSolved board = Just board
    | null values    = Nothing
    | null solutions = Nothing
    | otherwise      = head solutions
    where
        -- Auxiliary function, returns number of allowed values for a given cell
        numValues = length . getAllowedCellValues board
        -- Coordinates of a single empty cell with the lowest number of allowed values
        coord = head $ List.sortBy (Ord.comparing numValues) $ getEmptyCells board
        -- All allowed values for that cell
        values = getAllowedCellValues board coord
        -- Tries to put every allowed value in the cell and then solve the result
        solutions = filter (not . isNothing) $ map solve $ map (\v -> Map.insert coord v board) values

-- Checks whether puzzle is solved
isSolved :: Board -> Bool
isSolved board = null $ filter (== 0) $ Map.elems board

-- Utility functions -----------------------------------------------------------

-- Converts string to a Sudoku board
fromString :: String -> Maybe Board
fromString str = board
    where
        -- Split string to lines, remove whitespace and take first 9 non-empty lines
        ls = take 9 $ filter (not . null) $ map (filter (not . Char.isSpace)) $ lines str
        -- Is correct only if all lines are of length 9
        isCorrect = (length ls == 9) && (null $ filter ((/= 9) . length) ls)
        -- Converts any non-digit to a zero
        getCell x = if Char.isDigit x then read [x] else 0
        -- Builds a Board
        board
            | isCorrect = Just $ Map.fromList [((x, y), getCell $ ls !! (y - 1) !! (x - 1)) | x <- [1..9], y <- [1..9]]
            | otherwise = Nothing

-- Prints Sudoku board in a nice human-readable format
printBoard :: Board -> String
printBoard board =
    unlines $ map tail $ List.intercalate [line] $ map printRows coordGroups
    where coordGroups = [[], [1..3], [4..6], [7..9], []]
          line = " +-------+-------+-------+"
          printRows ys = map printRow ys
          printRow y = List.intercalate " | " $ map (printCells y) coordGroups
          printCells y xs = List.intercalate " " $ map (printCell y) xs
          printCell y x = if value == 0 then "." else show value
              where value = board ! (x,y)
