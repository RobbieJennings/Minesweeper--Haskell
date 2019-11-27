module UI where

import Minesweeper

import Data.List.Split
import System.Random

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as Core
import Graphics.Gloss.Interface.Environment

pixels :: Int -> String
pixels size = (show size) ++ "px"

show_cell :: Cell -> String
show_cell a
  | flagged a             = "F"
  | uncovered a && bomb a = "X"
  | uncovered a           = show (number a)
  | otherwise             = "?"

show_grid :: [Cell] -> [String]
show_grid a = map show_cell a

terminal :: [Cell] -> Int -> IO ()
terminal grid size
  | win grid size = putStrLn "You Win"
  | lose grid = putStrLn "You Lose"
  | otherwise = do
    putStrLn "Do you want to flag, uncover or ai?"
    action <- getLine
    if (action == "ai") then
      do
        let new_grid = ai grid
        mapM_ print $ chunksOf size $ show_grid new_grid
        terminal new_grid size
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
            terminal new_grid size
        else
          do
            mapM_ print $ chunksOf size $ show_grid grid
            terminal grid size
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
            terminal new_grid size
        else
          do
            mapM_ print $ chunksOf size $ show_grid grid
            terminal grid size
    else
      do
        terminal grid size

terminal_play :: Int -> Int -> IO ()
terminal_play size num_mines = do
  generator <- newStdGen
  let (bombs, _) = make_bombs size num_mines generator
  let grid = make_grid size bombs
  mapM_ print $ chunksOf size $ show_grid grid
  terminal grid size

cell_button :: [Cell] -> Int -> Int -> Int -> Window -> UI Element
cell_button grid size index button_size window
  | (not $ uncovered (grid!!index)) && (not $ flagged (grid!!index)) = do
      button <- UI.button # set text "."
                          # set (attr "class") ("button")
                          # set (attr "oncontextmenu") ("return false;")
                          # set style  [("background-color","white")
                                       , ("width", (pixels button_size))
                                       , ("height", (pixels button_size))
                                       ]
      return button
  | flagged (grid!!index) = do
      button <- UI.button # set text "F"
                          # set (attr "class") ("button")
                          # set (attr "oncontextmenu") ("return false;")
                          # set style  [("background-color","green")
                                       , ("width", (pixels button_size))
                                       , ("height", (pixels button_size))
                                       ]
      return button
  | bomb (grid!!index) = do
      button <- UI.button # set text "X"
                          # set (attr "class") ("button")
                          # set (attr "oncontextmenu") ("return false;")
                          # set style  [("background-color","red")
                                       , ("width", (pixels button_size))
                                       , ("height", (pixels button_size))
                                       ]
      return button
  | (number (grid!!index) == 0) = do
      button <- UI.button # set text "."
                          # set (attr "class") ("button")
                          # set (attr "oncontextmenu") ("return false;")
                          # set style  [("background-color","grey")
                                       , ("width", (pixels button_size))
                                       , ("height", (pixels button_size))
                                       ]
      return button
  | otherwise = do
      button <- UI.button # set text (show $ number (grid!!index))
                          # set (attr "class") ("button")
                          # set (attr "oncontextmenu") ("return false;")
                          # set style  [("background-color","grey")
                                       , ("width", (pixels button_size))
                                       , ("height", (pixels button_size))
                                       ]
      return button

clickable_cell :: [Cell] -> Int -> Int -> Int -> Int -> StdGen -> Window -> UI Element
clickable_cell grid size num_mines index button_size generator window = do
  button <- cell_button grid size index button_size window
  on UI.click button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    let new_grid = uncover grid index
    gui new_grid size num_mines button_size generator window
  on UI.contextmenu button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    let new_grid = flag grid index
    gui new_grid size num_mines button_size generator window
  return button

unclickable_cell :: [Cell] -> Int -> Int -> Int -> Window -> UI Element
unclickable_cell grid size index button_size window = do
  button <- cell_button grid size index button_size window
  return button

ai_button :: [Cell] -> Int -> Int -> Int -> StdGen -> Window -> UI Element
ai_button grid size num_mines button_size generator window = do
  button <- UI.button #+ [string "Make Move"]
                      # set (attr "class") ("button")
                      # set style [("width", (pixels (size * button_size))), ("height", (pixels button_size))]
  on UI.click button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    let new_grid = ai grid
    gui new_grid size num_mines button_size generator window
  return button

winner_button :: Int -> Int -> Int -> StdGen -> Window -> UI Element
winner_button size num_mines button_size generator window = do
  button <- UI.button #+ [string ":)"]
                      # set (attr "class") ("button")
                      # set style [("width", (pixels (size * button_size))), ("height", (pixels button_size))]
  on UI.click button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    start_gui size num_mines button_size generator window
  return button

loser_button :: [Cell] -> Int -> Int -> Int -> StdGen -> Window -> UI Element
loser_button grid size num_mines button_size generator window = do
  button <- UI.button #+ [string ":("]
                      # set (attr "class") ("button")
                      # set style [("width", (pixels (size * button_size))), ("height", (pixels button_size))]
  on UI.click button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    start_gui size num_mines button_size generator window
  return button

gui :: [Cell] -> Int -> Int -> Int -> StdGen -> Window -> UI ()
gui my_grid size num_mines button_size generator window
  | win my_grid size = do
      let button a = unclickable_cell my_grid size a button_size window
      let buttons = map button [0..(size^2)-1]
      button_grid <- grid $ chunksOf size buttons
      getBody window #+ [return button_grid]
      getBody window #+ [winner_button size num_mines button_size generator window]
      return ()
  | lose my_grid = do
      let button a = unclickable_cell my_grid size a button_size window
      let buttons = map button [0..(size^2)-1]
      button_grid <- grid $ chunksOf size buttons
      getBody window #+ [return button_grid]
      getBody window #+ [loser_button my_grid size num_mines button_size generator window]
      return ()
  | otherwise = do
      let button a = clickable_cell my_grid size num_mines a button_size generator window
      let buttons = map button [0..(size^2)-1]
      button_grid <- grid $ chunksOf size buttons
      getBody window #+ [return button_grid]
      getBody window #+ [ai_button my_grid size num_mines button_size generator window]
      return ()

start_gui :: Int -> Int -> Int -> StdGen -> Window -> UI ()
start_gui size num_mines button_size generator = do
  let (bombs, new_gen) = make_bombs size num_mines generator
  let grid = make_grid size bombs
  gui grid size num_mines button_size new_gen


gui_play :: Int -> Int -> IO ()
gui_play size num_mines = do
  (width, height) <- getScreenSize
  let screen_size = min width height
  let button_ratio = (screen_size `div` (size + 1))
  let button_size = floor (fromIntegral (button_ratio) * 0.8)

  generator <- getStdGen
  startGUI defaultConfig $ start_gui size num_mines button_size generator
