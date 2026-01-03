module Main where

import Data.Array
import qualified Data.Set as S
import Control.Monad.Random
import Control.Monad.ST.Trans
import qualified Control.Monad.Union as UF

import Data.Time.Clock.System (getSystemTime, SystemTime(..))
import System.Console.ANSI
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import PlayMaze

type Maze s = STT s (RandT StdGen (UF.UnionM (Int,Int)))

getMaze :: STArray s (Int,Int) Bool -> (Int,Int) -> Maze s Bool
getMaze maze (x,y) = do
  let ((xmin,ymin),(xmax,ymax)) = boundsSTArray maze
  if x < xmin || x > xmax || y < ymin || y > ymax
    then return True -- out of bounds is an empty space to aid printing
    else readSTArray maze (x,y)

printMazeChar :: Array (Int,Int) Bool -> (Int,Int) -> Char
printMazeChar maze (x,y) = do
  let b = maze ! (x,y)
  case b of
    True -> ' '
    False -> let
      ((xmin,ymin),(xmax,ymax)) = bounds maze
      above = if y == ymin then True else maze ! (x,y-1)
      below = if y == ymax then True else maze ! (x,y+1)
      left  = if x == xmin then True else maze ! (x-1,y)
      right = if x == xmax then True else maze ! (x+1,y)
      in case (above,below,left,right) of
{-
           -- light weight lines
           -- pillar
           (True,True,True,True) -> '\x2022' -- bullet
           -- end of wall
           (True,True,True,False) -> '\x2576'
           (True,True,False,True) -> '\x2574'
           (True,False,True,True) -> '\x2577'
           (False,True,True,True) -> '\x2575'
           -- middle of wall
           (False,False,True,True) -> '\x2502'
           (True,True,False,False) -> '\x2500'
           -- corners
           (True,False,True,False) -> '\x250c'
           (True,False,False,True) -> '\x2510'
           (False,True,True,False) -> '\x2514'
           (False,True,False,True) -> '\x2518'
           -- T intersections
           (False,False,False,True) -> '\x2524'
           (False,False,True,False) -> '\x251c'
           (False,True,False,False) -> '\x2534'
           (True,False,False,False) -> '\x252c'
           -- + intersction
           (False,False,False,False) -> '\x253c'
-}
           -- heavy weight lines
           -- pillar
           (True,True,True,True) -> '\x25cf'
           -- end of wall
           (True,True,True,False) -> '\x257a'
           (True,True,False,True) -> '\x2578'
           (True,False,True,True) -> '\x257b'
           (False,True,True,True) -> '\x2579'
           -- middle of wall
           (False,False,True,True) -> '\x2503'
           (True,True,False,False) -> '\x2501'
           -- corners
           (True,False,True,False) -> '\x250f'
           (True,False,False,True) -> '\x2513'
           (False,True,True,False) -> '\x2517'
           (False,True,False,True) -> '\x251b'
           -- T intersections
           (False,False,False,True) -> '\x252b'
           (False,False,True,False) -> '\x2523'
           (False,True,False,False) -> '\x253b'
           (True,False,False,False) -> '\x2533'
           -- + intersction
           (False,False,False,False) -> '\x254b'

prettyPrintRow :: Int -> Array (Int,Int) Bool -> String
prettyPrintRow y maze = do
  let ((xmin,_),(xmax,_)) = bounds maze
  map (\x -> printMazeChar maze (x,y)) [xmin..xmax]

prettyPrint :: Array (Int,Int) Bool -> Array (Int,Int) Char
prettyPrint maze =
  let ((xmin,ymin),(xmax,ymax)) = bounds maze
      chars = concat $ map (flip prettyPrintRow maze) [ymin..ymax]
  in listArray ((xmin,ymin),(xmax,ymax)) chars

clearSpace :: STArray s (Int,Int) Bool -> (Int,Int) -> Maze s ()
clearSpace maze (x,y) = writeSTArray maze (x,y) True

m :: l -> l -> (l,())
m x _ = (x,())

removeWalls :: STArray s (Int,Int) Bool -> S.Set (Int,Int) ->
               Array (Int,Int) UF.Node -> Int -> Maze s ()
removeWalls _ _ _ 0 = return ()
removeWalls maze walls places numToRemove = do
  let numWalls = S.size walls
  i <- lift $ getRandomR (0,numWalls-1)
  let wall = S.elemAt i walls
      (x,y) = wall
      newWalls = S.deleteAt i walls
      (side1,side2) = case x `mod` 2 of
        0 -> ((x-1,y),(x+1,y))
        _ -> ((x,y-1),(x,y+1))
  let node1 = places ! side1
      node2 = places ! side2
  area1 <- UF.lookup node1
  area2 <- UF.lookup node2
  newNumToRemove <- if area1 /= area2
    then do _ <- lift $ UF.merge m (fst area1) (fst area2)
            clearSpace maze wall
            return $ numToRemove - 1
    else return numToRemove
  removeWalls maze newWalls places newNumToRemove

makeMaze :: (Int,Int) -> Maze s (Array (Int,Int) Bool)
makeMaze (width,height) = do
  maze <- newSTArray ((0,0),(width-1,height-1)) False
  let startingSpaces = [(x,y) | x <- [1,3..width-2], y <- [1,3..height-2]]
  sequence_ $ map (clearSpace maze) startingSpaces
  clearSpace maze (0,1) -- start
  clearSpace maze (width-1,height-2) -- end
  let wallLocs = [(x,y) | x <- [1,3..width-2], y <- [2,4..height-3]] ++
                 [(x,y) | x <- [2,4..width-3], y <- [1,3..height-2]]
      walls = S.fromList wallLocs
  nodes <- sequence $ map UF.new startingSpaces
  let places = array ((0,0),(width-1,height-1)) $ zip startingSpaces nodes
  removeWalls maze walls places $ (width `div` 2) * (height `div` 2) - 1
  unsafeFreezeSTArray maze

addNewlines :: Int -> String -> String
addNewlines stride str =
  if length str <= stride
  then str ++ "\n"
  else take stride str ++ "\n" ++ addNewlines stride (drop stride str)

getNanosSinceEpoch :: IO Integer
getNanosSinceEpoch = do
    (MkSystemTime s ns) <- getSystemTime
    -- Convert seconds to nanoseconds and add the current nanosecond fraction
    return $ toInteger s * 10^(9 :: Int) + toInteger ns

main :: IO ()
main = do
  args <- getArgs
  (width,height,interactive) <- case args of
    [] -> do
      putStrLn "You may specify the size of the maze using command line arguments."
      putStrLn "For example: cabal run maze-maker -- 10 5"
      putStrLn "You can play through the maze on your computer with:"
      putStrLn "  cabal run maze-maker -- 20 20 play"
      putStrLn "The recommended font is \"Square\" by Wouter van Oortmerssen."
      putStr "Download it here: "
      hyperlink "https://strlen.com/square/" "https://strlen.com/square/"
      putStrLn "."
      return (5,5,False)
    [x,y] -> return (read x,read y,False)
    [x,y,"play"] -> return (read x,read y,True)
    _ -> hPutStrLn stderr "Please specify a width and a height." >> exitFailure
  if width <= 1 || height <= 1
    then hPutStrLn stderr "Width and height must be at least 2." >> exitFailure
    else return ()
  putStr $ setSGRCode [SetColor Foreground Vivid White,
                       SetColor Background Dull Black] ++
           clearFromCursorToScreenEndCode
  t <- getNanosSinceEpoch
  let myGen = mkStdGen $ fromInteger t
      result = UF.run $
        runRandT (runSTT (makeMaze (width*2+1,height*2+1))) myGen
      maze = fst result
      mazeChars = prettyPrint maze
  case interactive of
    False -> putStr $ addNewlines (width*2+1) $ elems mazeChars
    True -> playMaze mazeChars
  putStr $ setSGRCode [] ++ clearFromCursorToScreenEndCode
