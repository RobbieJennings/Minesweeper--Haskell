module Main where

import Minesweeper
import UI
import System.Random
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let size = read (args!!0) :: Int
  let num_mines = read (args!!1) :: Int

  bombs <- fmap (make_bombs size num_mines) getStdGen
  let grid = make_grid [] 0 0 size bombs

  -- Uncomment this line to play using threepenny GUI
  gui_play grid size

  -- Uncomment this line to play using terminal
  -- terminal_play grid size
