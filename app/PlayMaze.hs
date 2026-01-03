module PlayMaze (playMaze) where

import Control.Exception (bracket)
import Data.Array
import System.Console.ANSI
import System.IO

setup :: IO (BufferMode,Bool,BufferMode)
setup = do
  oldInputBuffering <- hGetBuffering stdin
  oldEcho <- hGetEcho stdin
  oldOutputBuffering <- hGetBuffering stdout
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  useAlternateScreenBuffer
  disableLineWrap
  return (oldInputBuffering,oldEcho,oldOutputBuffering)

cleanup :: (BufferMode,Bool,BufferMode) -> IO ()
cleanup (oldInputBuffering,oldEcho,oldOutputBuffering) = do
  enableLineWrap
  useNormalScreenBuffer
  hSetBuffering stdin oldInputBuffering
  hSetEcho stdin oldEcho
  hSetBuffering stdout oldOutputBuffering

playMaze :: Array (Int,Int) Char -> IO String
playMaze maze = bracket setup cleanup $ const $ playMaze' maze

playMaze' :: Array (Int,Int) Char -> IO String
playMaze' maze = do
  printMaze maze
  eventLoop maze (1,0)

printMaze :: Array (Int,Int) Char -> IO ()
printMaze maze = do
  clearScreen
  let ((ymin,xmin),(ymax,xmax)) = bounds maze
  sequence_ $ concat $
    [[setCursorPosition y 0] ++ [putChar (maze ! (y,x)) | x <- [xmin..xmax]]
    | y <- [ymin..ymax]]
  setCursorPosition (ymax+1) 0
  putStr "wasd or hjkl to move, q or ESC to quit"

eventLoop :: Array (Int,Int) Char -> (Int,Int) -> IO String
eventLoop maze (y,x) = do
  let ((_,_),(ymax,xmax)) = bounds maze
  if y == ymax-1 && x == xmax
    then return "Congratulations! You won!\n"
    else do
    setCursorPosition y x
    putChar '@'
    cursorBackward 1
    k <- getChar
    case k of
      'q' -> return ""
      '\x001b' -> return ""
      'h' -> goLeft maze (y,x)
      'l' -> goRight maze (y,x)
      'k' -> goUp maze (y,x)
      'j' -> goDown maze (y,x)
      'w' -> goUp maze (y,x)
      'a' -> goLeft maze (y,x)
      's' -> goDown maze (y,x)
      'd' -> goRight maze (y,x)
      _ -> eventLoop maze (y,x)

goLeft :: Array (Int,Int) Char -> (Int,Int) -> IO String
goLeft maze (y,x) = do
  putChar ' '
  let ((_,xmin),(_,_)) = bounds maze
      x' = if x > xmin then x-1 else x
      c = maze ! (y,x')
  eventLoop maze $ if c == ' ' then (y,x') else (y,x)

goRight :: Array (Int,Int) Char -> (Int,Int) -> IO String
goRight maze (y,x) = do
  putChar ' '
  let ((_,_),(_,xmax)) = bounds maze
      x' = if x < xmax then x+1 else x
      c = maze ! (y,x')
  eventLoop maze $ if c == ' ' then (y,x') else (y,x)

goUp :: Array (Int,Int) Char -> (Int,Int) -> IO String
goUp maze (y,x) = do
  putChar ' '
  let ((ymin,_),(_,_)) = bounds maze
      y' = if y > ymin then y-1 else y
      c = maze ! (y',x)
  eventLoop maze $ if c == ' ' then (y',x) else (y,x)

goDown :: Array (Int,Int) Char -> (Int,Int) -> IO String
goDown maze (y,x) = do
  putChar ' '
  let ((_,_),(ymax,_)) = bounds maze
      y' = if y < ymax then y+1 else y
      c = maze ! (y',x)
  eventLoop maze $ if c == ' ' then (y',x) else (y,x)
