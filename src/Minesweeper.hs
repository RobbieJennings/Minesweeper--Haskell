module Minesweeper where

import Data.List
import Data.List.Split
import System.Random

data Cell = Cell { uncovered    :: Bool
                 , flagged      :: Bool
                 , bomb         :: Bool
                 , number       :: Int
                 , neighbours   :: [Int]
                 }

get_index :: Int -> Int -> Int -> Int
get_index x y size = x+(y*size)

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

get_number :: Int -> Int -> Int -> [Int] -> Int
get_number x y size bombs = do
  let neighbours = get_neighbours x y size
  let adjacent_bombs = neighbours `intersect` bombs
  length adjacent_bombs

make_cell :: Int -> Int -> Int -> [Int] -> Cell
make_cell x y size bombs = do
  let bomb = is_bomb x y size bombs
  let neighbours = get_neighbours x y size
  let number = get_number x y size bombs
  Cell {uncovered=False, flagged=False, bomb=bomb, number=number, neighbours=neighbours}

make_randoms :: Int -> Int -> [Int] -> StdGen -> ([Int], StdGen)
make_randoms range count nums generator
  | (length nums) >= count = do
      let (_, new_gen) = randomR (0, (range-1)) generator
      (nums, new_gen)
  | otherwise = do
      let all_nums = [0..range]
      let available_nums = all_nums \\ nums
      let (index, new_gen) = randomR (0, (length available_nums) - 1) generator
      let num = available_nums!!index
      make_randoms range count (nums++[num]) new_gen

make_bombs :: Int -> Int -> StdGen -> ([Int], StdGen)
make_bombs size count generator = make_randoms (size^2) count [] generator

is_bomb :: Int -> Int -> Int -> [Int] -> Bool
is_bomb x y size bombs = (get_index x y size) `elem` bombs

make_grid :: Int -> [Int] -> [Cell]
make_grid size bombs = do
  let coordinates = [(y, x) | x <- [0..size-1], y <- [0..size-1]]
  let cell (x, y) = make_cell x y size bombs
  map cell coordinates

hidden_neighbours :: [Cell] -> Int -> [Int]
hidden_neighbours grid index = do
  let cell = grid!!index
  let all_neighbours = neighbours cell
  let hidden a = not $ uncovered $ grid!!a
  filter (hidden) all_neighbours

uncover_cell :: Cell -> Cell
uncover_cell a = Cell { uncovered = True
                      , flagged = False
                      , bomb = bomb a
                      , number = number a
                      , neighbours = neighbours a
                      }

uncover :: [Cell] -> Int -> [Cell]
uncover grid index = do
  let cell = uncover_cell $ grid!!index
  if ((number cell == 0) && (not $ bomb cell)) then
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

flag_cell :: Cell -> Cell
flag_cell a | uncovered a = a
            | flagged a   = Cell { uncovered = False
                                 , flagged = False
                                 , bomb = bomb a
                                 , neighbours = neighbours a
                                 , number = number a
                                 }
            | otherwise   = Cell { uncovered = False
                                 , flagged = True
                                 , bomb = bomb a
                                 , neighbours = neighbours a
                                 , number = number a
                                 }

flag :: [Cell] -> Int -> [Cell]
flag grid index = do
  let cell = flag_cell $ grid!!index
  let first = take index grid
  let second = drop (index+1) grid
  first ++ ([cell] ++ second)

win_cell :: Cell -> Bool
win_cell a = ((uncovered a) && (not (bomb a))) || ((not (uncovered a)) && (bomb a))

win :: [Cell] -> Int -> Bool
win grid size = do
  let winners = map win_cell grid
  let count = length $ filter (==True) winners
  count == (size^2)

lose_cell :: Cell -> Bool
lose_cell a = (uncovered a) && (bomb a)

lose :: [Cell] -> Bool
lose grid = do
  let losers = map lose_cell grid
  True `elem` losers

reset_cell :: Cell -> Cell
reset_cell a = Cell { uncovered = False
                     , flagged = False
                     , bomb = bomb a
                     , neighbours = neighbours a
                     , number = number a
                     }

reset :: [Cell] -> [Cell]
reset grid = map reset_cell grid

ai :: [Cell] -> [Cell]
ai grid = do
  uncover grid 0
