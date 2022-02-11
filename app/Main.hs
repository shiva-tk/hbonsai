module Main where

import Print
import System.Console.ANSI (getTerminalSize, 
                            setCursorPosition,
                            hideCursor,
                            showCursor)

main :: IO ()
main = do
  resetScreen
  size <- getTerminalSize
  case size of
    Nothing     -> return ()
    Just (h, w) -> do
      if w < 10 
        then return ()
        else do
          let startColumn = w `div` 4
          let startRow    = h - 6
          let width       = w `div` 2
          setCursorPosition startRow startColumn
          hideCursor
          drawPlantPot width
          holdUntilQuit
          showCursor
