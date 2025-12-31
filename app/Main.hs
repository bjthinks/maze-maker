module Main where

import Control.Monad.Union
import Control.Monad.Writer

type Maze = WriterT String (UnionM (Int,Int))

nl :: Maze ()
nl = tell "\n"

line :: String -> Maze ()
line s = tell s >> nl

test :: Maze ()
test = do
  line "testing union find monad"
  return ()

main :: IO ()
main = do
  putStr $ snd $ run $ runWriterT test
