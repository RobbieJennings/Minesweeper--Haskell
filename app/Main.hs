module Main where

import Data.List
import Data.List.Split
import System.Random

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as Core

data Cell = Cell { uncovered    :: Bool
                 , flagged      :: Bool
                 , bomb         :: Bool
                 , number       :: Int
                 , neighbours   :: [Int]
                 } deriving (Show)

uncover_cell :: Cell -> Cell
uncover_cell a = Cell { uncovered = True
                      , flagged = False
                      , bomb = bomb a
                      , number = number a
                      , neighbours = neighbours a
                      }

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
  let my_bomb = is_bomb x y size bombs
  let my_neighbours = get_neighbours x y size
  let my_number = get_number x y size bombs
  Cell {uncovered=False, flagged=False, bomb=my_bomb, number=my_number, neighbours=my_neighbours}

make_bombs :: Int -> Int -> StdGen -> [Int]
make_bombs size count gen = take count $ nub $ randomRs(0, (size^2)-1) gen :: [Int]

is_bomb :: Int -> Int -> Int -> [Int] -> Bool
is_bomb x y size bombs = (get_index x y size) `elem` bombs

make_grid :: [Cell] -> Int -> Int -> Int -> [Int] -> [Cell]
make_grid grid x y size bombs
  | y == size-1 && x == size-1 = grid++([make_cell x y size bombs])
  | x == size-1                = make_grid (grid++[(make_cell x y size bombs)]) 0 (y+1) size bombs
  | otherwise                  = make_grid (grid++[(make_cell x y size bombs)]) (x+1) (y) size bombs

hidden_neighbours :: [Cell] -> Int -> [Int]
hidden_neighbours grid index = do
  let my_cell = grid!!index
  let my_neighbours = neighbours my_cell
  let hidden a = not $ uncovered $ grid!!a
  filter (hidden) my_neighbours

uncover :: [Cell] -> Int -> [Cell]
uncover grid index = do
  let my_cell = uncover_cell $ grid!!index
  if ((number my_cell == 0) && (not $ bomb my_cell)) then
    do
      let first = take index grid
      let second = drop (index+1) grid
      let new_grid = first ++ ([my_cell] ++ second)
      let neighbours = hidden_neighbours grid index
      foldl uncover new_grid neighbours
  else
    do
      let first = take index grid
      let second = drop (index+1) grid
      first ++ ([my_cell] ++ second)

flag :: [Cell] -> Int -> [Cell]
flag grid index = do
  let my_cell = flag_cell $ grid!!index
  let first = take index grid
  let second = drop (index+1) grid
  first ++ ([my_cell] ++ second)

show_cell :: Cell -> String
show_cell a
  | flagged a             = "F"
  | uncovered a && bomb a = "X"
  | uncovered a           = show (number a)
  | otherwise             = "?"

show_grid :: [Cell] -> [String]
show_grid a = map show_cell a

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

ai :: [Cell] -> [Cell]
ai grid = do
  uncover grid 0

terminal_play :: [Cell] -> Int -> IO ()
terminal_play grid size
  | win grid size = putStrLn "You Win"
  | lose grid = putStrLn "You Lose"
  | otherwise = do
    putStrLn "Do you want to flag, uncover or ai?"
    action <- getLine
    if (action == "ai") then
      do
        let new_grid = ai grid
        mapM_ print $ chunksOf size $ show_grid new_grid
        terminal_play new_grid size
    else if (action == "flag") then
      do
        putStrLn "What is your x coordinate?"
        x_coordinate <- getLine
        putStrLn "What is your y coordinate?"
        y_coordinate <- getLine
        let x = read x_coordinate :: Int
        let y = read y_coordinate :: Int
        if (x < size && y < size) then
          do
            let index = get_index x y size
            let new_grid = flag grid index
            mapM_ print $ chunksOf size $ show_grid new_grid
            terminal_play new_grid size
        else
          do
            mapM_ print $ chunksOf size $ show_grid grid
            terminal_play grid size
    else if (action == "uncover") then
      do
        putStrLn "What is your x coordinate?"
        x_coordinate <- getLine
        putStrLn "What is your y coordinate?"
        y_coordinate <- getLine
        let x = read x_coordinate :: Int
        let y = read y_coordinate :: Int
        if (x < size && y < size) then
          do
            let index = get_index x y size
            let new_grid = flag grid index
            mapM_ print $ chunksOf size $ show_grid new_grid
            terminal_play new_grid size
        else
          do
            mapM_ print $ chunksOf size $ show_grid grid
            terminal_play grid size
    else
      do
        terminal_play grid size

clickable_cell :: [Cell] -> Int -> Int -> Window -> UI Element
clickable_cell grid size index window = do
  button <- UI.button #+ [string (show_cell (grid!!index))]
                      # set (attr "class") ("button")
  on UI.click button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    let new_grid = uncover grid index
    gui_play new_grid size window
  on UI.contextmenu button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    let new_grid = flag grid index
    gui_play new_grid size window
  return button

unclickable_cell :: [Cell] -> Int -> Int -> Window -> UI Element
unclickable_cell grid size index window = do
  button <- UI.button #+ [string (show_cell (grid!!index))]
                      # set (attr "class") ("button")
  return button

ai_button :: [Cell] -> Int -> Window -> UI Element
ai_button grid size window = do
  let width = (show (size*24)) ++ "px"
  let height = (show 24) ++ "px"
  button <- UI.button #+ [string "AI"]
                      # set (attr "class") ("button")
                      # set style [("width", width), ("height", height)]
  on UI.click button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    let new_grid = ai grid
    gui_play new_grid size window
  return button

winner_button :: Int -> Window -> UI Element
winner_button size window = do
  let width = (show (size*24)) ++ "px"
  let height = (show 24) ++ "px"
  button <- UI.button #+ [string "WINNER"]
                      # set (attr "class") ("button")
                      # set style [("width", width), ("height", height)]
  return button

loser_button :: Int -> Window -> UI Element
loser_button size window = do
  let w = (show (size*24)) ++ "px"
  let h = (show 24) ++ "px"
  button <- UI.button #+ [string "LOSER"]
                      # set (attr "class") ("button")
                      # set style [("width",w),("height",h)]
  return button

gui_play :: [Cell] -> Int -> Window -> UI ()
gui_play my_grid size window
  | win my_grid size = do
      let button a = unclickable_cell my_grid size a window
      let buttons = map button [0..(size^2)-1]
      button_grid <- grid $ chunksOf size buttons
      getBody window #+ [return button_grid]
      getBody window #+ [winner_button size window]
      return ()
  | lose my_grid = do
      let button a = unclickable_cell my_grid size a window
      let buttons = map button [0..(size^2)-1]
      button_grid <- grid $ chunksOf size buttons
      getBody window #+ [return button_grid]
      getBody window #+ [loser_button size window]
      return ()
  | otherwise = do
      let button a = clickable_cell my_grid size a window
      let buttons = map button [0..(size^2)-1]
      button_grid <- grid $ chunksOf size buttons
      getBody window #+ [return button_grid]
      getBody window #+ [ai_button my_grid size window]
      return ()

main :: IO ()
main = do
  let size = 5
  let num_mines = 5
  bombs <- fmap (make_bombs size num_mines) getStdGen
  let grid = make_grid [] 0 0 size bombs

  -- Uncomment this line to play using threepenny GUI
  startGUI defaultConfig $ gui_play grid size

  -- Uncomment these lines to play using terminal
  -- mapM_ print $ chunksOf size $ show_grid grid
  -- terminal_play grid size
