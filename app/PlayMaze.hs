module PlayMaze (playMaze) where

import Control.Exception (bracket)
import Data.Array
import System.IO
import Control.Concurrent (threadDelay)

setup :: IO (BufferMode,Bool,BufferMode)
setup = do
  oldInputBuffering <- hGetBuffering stdin
  oldEcho <- hGetEcho stdin
  oldOutputBuffering <- hGetBuffering stdout
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  return (oldInputBuffering,oldEcho,oldOutputBuffering)

cleanup :: (BufferMode,Bool,BufferMode) -> IO ()
cleanup (oldInputBuffering,oldEcho,oldOutputBuffering) = do
  hSetBuffering stdin oldInputBuffering
  hSetEcho stdin oldEcho
  hSetBuffering stdout oldOutputBuffering

playMaze :: Array (Int,Int) Char -> IO ()
playMaze maze = bracket setup cleanup $ const $ playMaze' maze

playMaze' :: Array (Int,Int) Char -> IO ()
playMaze' _ = do
  putStr "Delaying for 3 seconds"
  threadDelay 3000000
