module PlayMaze (playMaze) where

import Control.Exception (bracket)
import Data.Array
import Graphics.Vty
import Graphics.Vty.CrossPlatform
import System.Console.ANSI
import System.IO

setup :: IO (Vty,BufferMode)
setup = do
  vty <- mkVty defaultConfig
  oldOutputBuffering <- hGetBuffering stdout
  hSetBuffering stdout NoBuffering
  useAlternateScreenBuffer
  disableLineWrap
  return (vty,oldOutputBuffering)

cleanup :: (Vty,BufferMode) -> IO ()
cleanup (vty,oldOutputBuffering) = do
  shutdown vty
  enableLineWrap
  useNormalScreenBuffer
  hSetBuffering stdout oldOutputBuffering

playMaze :: Array (Int,Int) Char -> IO String
playMaze maze = bracket setup cleanup $ playMaze' maze

playMaze' :: Array (Int,Int) Char -> (Vty,BufferMode) -> IO String
playMaze' maze (vty,_) = do
  printMaze maze
  eventLoop vty maze (1,0)

printMaze :: Array (Int,Int) Char -> IO ()
printMaze maze = do
  clearScreen
  let ((ymin,xmin),(ymax,xmax)) = bounds maze
  sequence_ $ concat $
    [[setCursorPosition y 0] ++ [putChar (maze ! (y,x)) | x <- [xmin..xmax]]
    | y <- [ymin..ymax]]
  setCursorPosition (ymax+1) 0
  putStr "arrows, wasd, or hjkl to move, q or ESC to quit"

eventLoop :: Vty -> Array (Int,Int) Char -> (Int,Int) -> IO String
eventLoop vty maze (y,x) = do
  let ((_,_),(ymax,xmax)) = bounds maze
  if y == ymax-1 && x == xmax
    then return "Congratulations! You won!\n"
    else do
    setCursorPosition y x
    putChar '@'
    cursorBackward 1
    e <- nextEvent vty
    case e of
      EvKey (KChar 'q') [] -> return ""
      EvKey KEsc [] -> return ""
      EvKey (KChar 'h') [] -> goLeft vty maze (y,x)
      EvKey (KChar 'l') [] -> goRight vty maze (y,x)
      EvKey (KChar 'k') [] -> goUp vty maze (y,x)
      EvKey (KChar 'j') [] -> goDown vty maze (y,x)
      EvKey (KChar 'w') [] -> goUp vty maze (y,x)
      EvKey (KChar 'a') [] -> goLeft vty maze (y,x)
      EvKey (KChar 's') [] -> goDown vty maze (y,x)
      EvKey (KChar 'd') [] -> goRight vty maze (y,x)
      EvKey KUp    [] -> goUp vty maze (y,x)
      EvKey KLeft  [] -> goLeft vty maze (y,x)
      EvKey KDown  [] -> goDown vty maze (y,x)
      EvKey KRight [] -> goRight vty maze (y,x)
      _ -> eventLoop vty maze (y,x)

goLeft :: Vty -> Array (Int,Int) Char -> (Int,Int) -> IO String
goLeft vty maze (y,x) = do
  putChar ' '
  let ((_,xmin),(_,_)) = bounds maze
      x' = if x > xmin then x-1 else x
      c = maze ! (y,x')
  eventLoop vty maze $ if c == ' ' then (y,x') else (y,x)

goRight :: Vty -> Array (Int,Int) Char -> (Int,Int) -> IO String
goRight vty maze (y,x) = do
  putChar ' '
  let ((_,_),(_,xmax)) = bounds maze
      x' = if x < xmax then x+1 else x
      c = maze ! (y,x')
  eventLoop vty maze $ if c == ' ' then (y,x') else (y,x)

goUp :: Vty -> Array (Int,Int) Char -> (Int,Int) -> IO String
goUp vty maze (y,x) = do
  putChar ' '
  let ((ymin,_),(_,_)) = bounds maze
      y' = if y > ymin then y-1 else y
      c = maze ! (y',x)
  eventLoop vty maze $ if c == ' ' then (y',x) else (y,x)

goDown :: Vty -> Array (Int,Int) Char -> (Int,Int) -> IO String
goDown vty maze (y,x) = do
  putChar ' '
  let ((_,_),(ymax,_)) = bounds maze
      y' = if y < ymax then y+1 else y
      c = maze ! (y',x)
  eventLoop vty maze $ if c == ' ' then (y',x) else (y,x)
