module PlayMaze (playMaze) where

import Control.Exception (bracket)
import Data.Array
import System.IO
import Control.Concurrent (threadDelay)

setup :: IO (BufferMode,Bool)
setup = do
  oldBuffering <- hGetBuffering stdin
  oldEcho <- hGetEcho stdin
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  return (oldBuffering,oldEcho)

cleanup :: (BufferMode,Bool) -> IO ()
cleanup (oldBuffering,oldEcho) = do
  hSetBuffering stdin oldBuffering
  hSetEcho stdin oldEcho

playMaze :: Array (Int,Int) Char -> IO ()
playMaze maze = bracket setup cleanup $ const $ playMaze' maze

playMaze' :: Array (Int,Int) Char -> IO ()
playMaze' _ = do
  putStrLn "Delaying for 3 seconds"
  threadDelay 3000000
