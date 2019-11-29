module Minesweeper where

import Data.List
import Data.List.Split
import System.Random
import Control.Monad

data Cell = Cell { uncovered    :: Bool
                 , flagged      :: Bool
                 , mine         :: Bool
                 , number       :: Int
                 , neighbours   :: [Int]
                 }

-- Converts x and y coordinates to index in grid
get_index :: Int -> Int -> Int -> Int
get_index x y size = x+(y*size)

-- Gets list of indices of neighbouring cells in a grid using x and y coordinates
get_neighbours :: Int -> Int -> Int -> [Int]
get_neighbours x y size
  | size < 2                    = []
  | x == 0 && y == 0            = [(get_index x (y+1) size), (get_index (x+1) y size), (get_index (x+1) (y+1) size)]
  | x == 0 && y == size-1       = [(get_index x (y-1) size), (get_index (x+1) (y-1) size), (get_index (x+1) y size)]
  | x == size-1 && y == 0       = [(get_index (x-1) y size), (get_index (x-1) (y+1) size), (get_index x (y+1) size)]
  | x == size-1 && y == size-1  = [(get_index (x-1) (y-1) size), (get_index (x-1) y size), (get_index x (y-1) size)]
  | x == 0                      = [(get_index x (y-1) size), (get_index x (y+1) size), (get_index (x+1) (y-1) size), (get_index (x+1) y size), (get_index (x+1) (y+1) size)]
  | x == size-1                 = [(get_index (x-1) (y-1) size), (get_index (x-1) y size), (get_index (x-1) (y+1) size), (get_index x (y-1) size), (get_index x (y+1) size)]
  | y == 0                      = [(get_index (x-1) y size), (get_index (x-1) (y+1) size), (get_index x (y+1) size), (get_index (x+1) y size), (get_index (x+1) (y+1) size)]
  | y == size-1                 = [(get_index (x-1) (y-1) size), (get_index (x-1) y size), (get_index x (y-1) size), (get_index (x+1) (y-1) size), (get_index (x+1) y size)]
  | otherwise                   = [(get_index (x-1) (y-1) size), (get_index (x-1) y size), (get_index (x-1) (y+1) size), (get_index x (y-1) size), (get_index x (y+1) size), (get_index (x+1) (y-1) size), (get_index (x+1) y size), (get_index (x+1) (y+1) size)]

-- Gets number of neighbouring mines of a cell in a grid using x and y coordinates
get_number :: Int -> Int -> Int -> [Int] -> Int
get_number x y size mines = do
  let neighbours = get_neighbours x y size
  let adjacent_mines = neighbours `intersect` mines
  length adjacent_mines

-- Generates a random list of unique indices for mines and produces new generator
make_randoms :: Int -> Int -> [Int] -> StdGen -> ([Int], StdGen)
make_randoms range count nums generator
  | (length nums) >= count = do
      let (_, new_generator) = randomR (0, (range-1)) generator
      (nums, new_generator)
  | otherwise = do
      let all_nums = [0..range]
      let available_nums = all_nums \\ nums
      let (index, new_generator) = randomR (0, (length available_nums) - 1) generator
      let num = available_nums!!index
      make_randoms range count (nums++[num]) new_generator

-- Makes a list of random mines and a new generator
make_mines :: Int -> Int -> StdGen -> ([Int], StdGen)
make_mines size count generator = make_randoms (size^2) count [] generator

-- Checks if x and y coordinates correspond to a mine
is_mine :: Int -> Int -> Int -> [Int] -> Bool
is_mine x y size mines = (get_index x y size) `elem` mines

-- Makes a cell using x and y coordinates and mine array
make_cell :: Int -> Int -> Int -> [Int] -> Cell
make_cell x y size mines = do
  let mine = is_mine x y size mines
  let neighbours = get_neighbours x y size
  let number = get_number x y size mines
  Cell {uncovered=False, flagged=False, mine=mine, number=number, neighbours=neighbours}

-- Makes a grid of cells
make_grid :: Int -> [Int] -> [Cell]
make_grid size mines = do
  let coordinates = [(y, x) | x <- [0..size-1], y <- [0..size-1]]
  let cell (x, y) = make_cell x y size mines
  map cell coordinates

-- Finds all neighbours of a cell in a grid that have yet to be uncovered
hidden_neighbours :: [Cell] -> Int -> [Int]
hidden_neighbours grid index = do
  let cell = grid!!index
  let all_neighbours = neighbours cell
  let hidden a = not $ uncovered $ grid!!a
  filter (hidden) all_neighbours

-- Finds all neighbours of a cell in a grid that have been flagged
flagged_neighbours :: [Cell] -> Int -> [Int]
flagged_neighbours grid index = do
  let cell = grid!!index
  let all_neighbours = neighbours cell
  let flag a = flagged $ grid!!a
  filter (flag) all_neighbours

-- Uncovers individual cell
uncover_cell :: Cell -> Cell
uncover_cell a = Cell { uncovered = True
                      , flagged = False
                      , mine = mine a
                      , number = number a
                      , neighbours = neighbours a
                      }

-- Uncovers cell in grid and propogates if number is 0
uncover :: [Cell] -> Int -> [Cell]
uncover grid index = do
  let cell = uncover_cell $ grid!!index
  if ((number cell == 0) && (not $ mine cell)) then
    do
      let first = take index grid
      let second = drop (index+1) grid
      let new_grid = first ++ ([cell] ++ second)
      let neighbours = hidden_neighbours grid index
      foldl uncover new_grid neighbours
  else
    do
      let first = take index grid
      let second = drop (index+1) grid
      first ++ ([cell] ++ second)

-- Flags individual cell
flag_cell :: Cell -> Cell
flag_cell a | uncovered a = a
            | flagged a   = Cell { uncovered = False
                                 , flagged = False
                                 , mine = mine a
                                 , neighbours = neighbours a
                                 , number = number a
                                 }
            | otherwise   = Cell { uncovered = False
                                 , flagged = True
                                 , mine = mine a
                                 , neighbours = neighbours a
                                 , number = number a
                                 }

-- Flags cell in grid
flag :: [Cell] -> Int -> [Cell]
flag grid index = do
  let cell = flag_cell $ grid!!index
  let first = take index grid
  let second = drop (index+1) grid
  first ++ ([cell] ++ second)

-- Checks if individual cell satisfies winning condition
win_cell :: Cell -> Bool
win_cell a = ((uncovered a) && (not (mine a))) || ((not (uncovered a)) && (mine a))

-- Checks if all cells in grid satisfy winning condition
win :: [Cell] -> Int -> Bool
win grid size = do
  let winners = map win_cell grid
  let count = length $ filter (==True) winners
  count == (size^2)

-- Checks if indivual cell satisfies losing condition
lose_cell :: Cell -> Bool
lose_cell a = (uncovered a) && (mine a)

-- Checks if all cells in grid satisfy losing condition
lose :: [Cell] -> Bool
lose grid = do
  let losers = map lose_cell grid
  True `elem` losers

-- Resets indivual cell to uncovered state
reset_cell :: Cell -> Cell
reset_cell a = Cell { uncovered = False
                     , flagged = False
                     , mine = mine a
                     , neighbours = neighbours a
                     , number = number a
                     }

-- Resets all cells in grid to uncovered state
reset :: [Cell] -> [Cell]
reset grid = map reset_cell grid

-- Finds all neighbours of a cell that are potential mines
potential_neighbours :: [Cell] -> Int -> [Int]
potential_neighbours grid index = do
  let hidden = hidden_neighbours grid index
  let flagged = flagged_neighbours grid index
  hidden \\ flagged

-- Checks if cell is hidden and neighbouring a revealed cell with a number greater than 0
-- This means we can reasonably calculate a probability of this cell being a mine.
potential_cell :: [Cell] -> Int -> Bool
potential_cell grid index
  | (uncovered (grid!!index)) || (flagged (grid!!index)) = False
  | otherwise = do
    let is_number a = (uncovered (grid!!a)) && ((number (grid!!a)) > 0)
    let numbers = map is_number (neighbours (grid!!index))
    True `elem` numbers

-- Finds all potential cells in a grid
potential_cells :: [Cell] -> Int -> [Int]
potential_cells grid size = filter (potential_cell grid) [0..(size^2)-1]

-- Finds all cells that are not uncovered and not flagged in a grid
hidden_cells :: [Cell] -> Int -> [Int]
hidden_cells grid size = do
  let is_hidden a = (not $ uncovered $ grid!!a) && (not $ flagged $ grid!!a)
  filter is_hidden [0..(size^2)-1]

-- Finds all uncovered cells that have neighbouring mines in a grid
number_cells :: [Cell] -> Int -> [Int]
number_cells grid size = do
  let indices = [0..((size^2)-1)]
  let numbered_cell a = (uncovered $ grid!!a) && ((number $ grid!!a) > 0)
  filter (numbered_cell) indices

-- Gets probability of all neighbouring hidden cells being a bomb
-- Returns probability and list of neighbouring indices
probability_cell :: [Cell] -> Int -> ([Int], Float)
probability_cell grid index = do
  let cell_number = number (grid!!index)
  let potential_mines = potential_neighbours grid index
  let probability = (fromIntegral cell_number) / (fromIntegral $ length potential_mines)
  (potential_mines, probability)

-- Gets probability of every cell being a bomb by averaging probabilities for each cell
probability_grid :: [Cell] -> Int -> [(Int, Float)]
probability_grid grid size = do
  let default_probability a = (a, 0.5)
  let probabilities = map (probability_cell grid) (number_cells grid size)
  let occurs index a = index `elem` (fst a)
  let occurances a = filter (occurs a) probabilities
  let get_probabilities a = map (snd) (occurances a)
  let get_average a = (sum $ get_probabilities a) / (fromIntegral $ length $ get_probabilities a)
  let get_tuple a = (a, (get_average a))
  let known_probabilities = map get_tuple (potential_cells grid size)
  let unknowns = (hidden_cells grid size) \\ (map fst known_probabilities)
  let unknown_probabilities = map default_probability unknowns
  known_probabilities ++ unknown_probabilities

-- Makes safest possible move in a grid of cells
ai :: [Cell] -> Int -> [Cell]
ai grid size = do
  let probabilities = probability_grid grid size
  let minimum_probability a = (snd a) == (minimum $ map snd probabilities)
  let maximum_probability a = (snd a) == (maximum $ map snd probabilities)
  let safest_uncover = head $ filter (minimum_probability) probabilities
  let safest_flag = head $ filter (minimum_probability) probabilities
  if (snd safest_flag) == 1 then
    flag grid (fst safest_flag)
  else
    uncover grid (fst safest_uncover)
