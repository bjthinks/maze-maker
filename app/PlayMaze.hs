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
  useNormalScreenBuffer
  hSetBuffering stdin oldInputBuffering
  hSetEcho stdin oldEcho
  hSetBuffering stdout oldOutputBuffering

playMaze :: Array (Int,Int) Char -> IO ()
playMaze maze = bracket setup cleanup $ const $ playMaze' maze

playMaze' :: Array (Int,Int) Char -> IO ()
playMaze' maze = do
  printMaze maze
  eventLoop maze

printMaze :: Array (Int,Int) Char -> IO ()
printMaze maze = do
  return ()

eventLoop :: Array (Int,Int) Char -> IO ()
eventLoop maze = do
  k <- getChar
  if k == 'q' || k == '\x001b'
    then return ()
    else eventLoop maze
