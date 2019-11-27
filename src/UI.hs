module UI where

import Minesweeper
import Data.List.Split
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as Core

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

terminal_play :: [Cell] -> Int -> IO ()
terminal_play grid size = do
  mapM_ print $ chunksOf size $ show_grid grid
  terminal grid size

clickable_cell :: [Cell] -> Int -> Int -> Window -> UI Element
clickable_cell grid size index window = do
  button <- UI.button #+ [string (show_cell (grid!!index))]
                      # set (attr "class") ("button")
  on UI.click button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    let new_grid = uncover grid index
    gui new_grid size window
  on UI.contextmenu button $ \_ -> do
    buttons <- getElementsByClassName window "button"
    mapM_ Core.delete buttons
    let new_grid = flag grid index
    gui new_grid size window
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
    gui new_grid size window
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
  let width = (show (size*24)) ++ "px"
  let height = (show 24) ++ "px"
  button <- UI.button #+ [string "LOSER"]
                      # set (attr "class") ("button")
                      # set style [("width", width), ("height", height)]
  return button

gui :: [Cell] -> Int -> Window -> UI ()
gui my_grid size window
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

gui_play :: [Cell] -> Int -> IO ()
gui_play grid size = startGUI defaultConfig $ gui grid size
