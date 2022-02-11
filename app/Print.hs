module Print where

import System.Console.ANSI
import System.IO

-- Clears screen, returns cursor to (0,0)
resetScreen :: IO ()
resetScreen 
  = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

-- Sets the foreground to given vivid colour to a given colour
setColor :: Color -> IO ()
setColor c = setSGR [SetColor Foreground Vivid c]

-- Waits for character input from the user,
-- then clears the screen. Used to quit the program.
holdUntilQuit :: IO ()
holdUntilQuit = do
  hSetBuffering stdin NoBuffering
  _ <- getChar
  resetScreen

-- Some strings to help with printing the bonsai
potLeg, trunkBase :: String
trunkBase = "./~~~\\."

potLeg = "(_)"

potBase, moss :: Char
moss = '_'

potBase = '_'

-- Some ints to help with readability.
trunkBaseWidth :: Int
trunkBaseWidth = 7

-- Draws a plant pot, from the current cursor position.
-- Takes the width of the plantpot as an arguement.
-- Width is the width at the top of the plant pot.
-- Pre : width is at least 10.
drawPlantPot :: Int -> IO ()
drawPlantPot width = do
  pos <- getCursorPosition
  case pos of
    Nothing       -> return ()
    Just (_, col) -> do
      -- Generate mossy layer on top of pot.
      setSGR [SetColor Foreground Dull Green]
      let numLeftMoss  = (width - trunkBaseWidth) `div` 2
      let numRightMoss = width - trunkBaseWidth - numLeftMoss
      let leftMoss     = replicate numLeftMoss moss
      let rightMoss    = replicate numRightMoss moss
      -- Draw mossy layer.
      setColor Green
      putStr leftMoss
      setColor Yellow
      putStr trunkBase
      setColor Green
      putStrLn rightMoss
      setCursorColumn col
      -- Generate pot.
      let potLine1     = "\\" ++ replicate (width - 2) ' ' ++ "/"
      let potLine2     = " \\" ++ replicate (width - 4) potBase ++ "/"
      let legs         = " " ++ potLeg ++ replicate (width - 8) ' ' ++ potLeg
      -- Draw pot.
      setColor Black
      putStrLn potLine1
      setCursorColumn col
      putStrLn potLine2
      setCursorColumn col
      putStrLn legs
      setColor White

