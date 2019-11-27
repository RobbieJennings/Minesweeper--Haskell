module Main where

import Minesweeper
import UI
import System.Random

main :: IO ()
main = do
  let size = 5
  let num_mines = 5

  bombs <- fmap (make_bombs size num_mines) getStdGen
  let grid = make_grid [] 0 0 size bombs

  -- Uncomment this line to play using threepenny GUI
  gui_play grid size

  -- Uncomment this line to play using terminal
  -- terminal_play grid size
