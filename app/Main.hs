module Main where

import Data.Time.Clock.System
import Control.Monad.Random
import Control.Monad.ST.Trans
import qualified Control.Monad.Union as UF
import Control.Monad.Writer

type Maze s = STT s (RandT StdGen (WriterT String (UF.UnionM (Int,Int))))

nl :: Maze s ()
nl = tell "\n"

line :: String -> Maze s ()
line s = tell s >> nl

m :: l -> l -> (l,())
m x _ = (x,())

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
  nodes <- sequence $ map UF.new [(2,1),(2,3),(1,2),(3,2)]
  _ <- UF.merge m (nodes !! 0) (nodes !! 1)
  _ <- UF.merge m (nodes !! 2) (nodes !! 3)
  labels <- sequence $ map UF.lookup nodes
  line $ "Nodes: " ++ show (map snd labels)
  return ()

getNanosSinceEpoch :: IO Integer
getNanosSinceEpoch = do
    (MkSystemTime s ns) <- getSystemTime
    -- Convert seconds to nanoseconds and add the current nanosecond fraction
    return $ toInteger s * 10^(9 :: Int) + toInteger ns

main :: IO ()
main = do
  t <- getNanosSinceEpoch
  putStr $ snd $ UF.run $ runWriterT $ runRandT (runSTT test) $
    mkStdGen $ fromInteger t
