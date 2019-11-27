module Main where

import Minesweeper
import UI
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let size = read (args!!0) :: Int
  let num_mines = read (args!!1) :: Int

  -- Uncomment this line to play using threepenny GUI
  gui_play size num_mines

  -- Uncomment this line to play using terminal
  -- terminal_play size num_mines
