module Main where

import Data.Time.Clock.System
import Control.Monad.Random
import Control.Monad.ST.Trans
import Control.Monad.Union
import Control.Monad.Writer

type Maze s = STT s (RandT StdGen (WriterT String (UnionM (Int,Int))))

nl :: Maze s ()
nl = tell "\n"

line :: String -> Maze s ()
line s = tell s >> nl

test :: Maze s ()
test = do
  line "testing Maze monad"
  x <- lift getRandom :: Maze s Int
  line $ "Random Int: " ++ show x
  a <- newSTArray (0,9 :: Int) (0 :: Int)
  writeSTArray a 0 10
  writeSTArray a 1 100
  writeSTArray a 9 42
  f <- freezeSTArray a
  line $ "Array: " ++ show f
  return ()

getNanosSinceEpoch :: IO Integer
getNanosSinceEpoch = do
    (MkSystemTime s ns) <- getSystemTime
    -- Convert seconds to nanoseconds and add the current nanosecond fraction
    return $ toInteger s * 10^(9 :: Int) + toInteger ns

main :: IO ()
main = do
  t <- getNanosSinceEpoch
  putStr $ snd $ run $ runWriterT $ runRandT (runSTT test) $
    mkStdGen $ fromInteger t
