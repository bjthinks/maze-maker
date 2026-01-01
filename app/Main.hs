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
    then return True -- out of bounds is an empty space to aid printing
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
        (True,True,True,True) -> tell "\x2022" -- bullet
        -- end of wall
        (True,True,True,False) -> tell "\x2576"
        (True,True,False,True) -> tell "\x2574"
        (True,False,True,True) -> tell "\x2577"
        (False,True,True,True) -> tell "\x2575"
        -- middle of wall
        (False,False,True,True) -> tell "\x2502"
        (True,True,False,False) -> tell "\x2500"
        -- corners
        (True,False,True,False) -> tell "\x250c"
        (True,False,False,True) -> tell "\x2510"
        (False,True,True,False) -> tell "\x2514"
        (False,True,False,True) -> tell "\x2518"
        -- T intersections
        (False,False,False,True) -> tell "\x2524"
        (False,False,True,False) -> tell "\x251c"
        (False,True,False,False) -> tell "\x2534"
        (True,False,False,False) -> tell "\x252c"
        -- + intersction
        (False,False,False,False) -> tell "\x253c"

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
  xx <- lift getRandom :: Maze s Int
  line $ "Random Int: " ++ show xx
  nodes <- sequence $ map UF.new [(2,1),(2,3),(1,2),(3,2)]
  _ <- UF.merge m (nodes !! 0) (nodes !! 1)
  _ <- UF.merge m (nodes !! 2) (nodes !! 3)
  labels <- sequence $ map UF.lookup nodes
  line $ "Nodes: " ++ show (map snd labels)
  line ""
  let width = 13
      height = 5
  maze <- newSTArray ((0,0),(width-1,height-1)) False
  let startingSpaces = [(x,y) | x <- [1,3..width-2], y <- [1,3..height-2]]
  sequence_ $ map (clearSpace maze) startingSpaces
  sequence_ $ map (clearSpace maze) [(2,2),(6,1),(6,3),(5,2),(7,2)]
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
