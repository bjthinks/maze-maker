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

getMaze :: STArray s (Int,Int) Bool -> (Int,Int) -> Maze s Bool
getMaze maze (x,y) = do
  let ((xmin,ymin),(xmax,ymax)) = boundsSTArray maze
  if x < xmin || x > xmax || y < ymin || y > ymax
    then return True
    else readSTArray maze (x,y)

printMazeChar :: STArray s (Int,Int) Bool -> (Int,Int) -> Maze s ()
printMazeChar maze (x,y) = do
  b <- getMaze maze (x,y)
  case b of
    True -> tell " "
    False -> do
      above <- getMaze maze (x,y-1)
      below <- getMaze maze (x,y+1)
      left  <- getMaze maze (x-1,y)
      right <- getMaze maze (x+1,y)
      case (above,below,left,right) of
        -- pillar
        (True,True,True,True) -> tell "0"
        -- end of wall
        (True,True,True,False) -> tell "-"
        (True,True,False,True) -> tell "-"
        (True,False,True,True) -> tell "|"
        (False,True,True,True) -> tell "|"
        -- middle of wall
        (False,False,True,True) -> tell "|"
        (True,True,False,False) -> tell "-"
        -- corners
        (True,False,True,False) -> tell "+"
        (True,False,False,True) -> tell "+"
        (False,True,True,False) -> tell "+"
        (False,True,False,True) -> tell "+"
        -- T intersections
        (False,False,False,True) -> tell "+"
        (False,False,True,False) -> tell "+"
        (False,True,False,False) -> tell "+"
        (True,False,False,False) -> tell "+"
        -- + intersction
        (False,False,False,False) -> tell "+"

prettyPrintRow :: Int -> STArray s (Int,Int) Bool -> Maze s ()
prettyPrintRow y maze = do
  let ((xmin,_),(xmax,_)) = boundsSTArray maze
  sequence_ $ map (\x -> printMazeChar maze (x,y)) [xmin..xmax]
  nl

prettyPrint :: STArray s (Int,Int) Bool -> Maze s ()
prettyPrint maze = do
  let ((_,ymin),(_,ymax)) = boundsSTArray maze
  sequence_ $ map (flip prettyPrintRow maze) [ymin..ymax]

clearSpace :: STArray s (Int,Int) Bool -> (Int,Int) -> Maze s ()
clearSpace maze (x,y) = writeSTArray maze (x,y) True

m :: l -> l -> (l,())
m x _ = (x,())

test :: Maze s ()
test = do
  line "testing Maze monad"
  xx <- lift getRandom :: Maze s Int
  line $ "Random Int: " ++ show xx
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
  line ""
  let width = 7
      height = 5
  maze <- newSTArray ((0,0),(width-1,height-1)) False
  let startingSpaces = [(x,y) | x <- [1,3..width-2], y <- [1,3..height-2]]
  sequence_ $ map (clearSpace maze) startingSpaces
  prettyPrint maze

getNanosSinceEpoch :: IO Integer
getNanosSinceEpoch = do
    (MkSystemTime s ns) <- getSystemTime
    -- Convert seconds to nanoseconds and add the current nanosecond fraction
    return $ toInteger s * 10^(9 :: Int) + toInteger ns

main :: IO ()
main = do
  t <- getNanosSinceEpoch
  let myGen = mkStdGen $ fromInteger t
      result = UF.run $ runWriterT $ runRandT (runSTT test) myGen
  putStr $ snd result
