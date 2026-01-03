module PlayMaze (playMaze) where

import Control.Exception (bracket)
import Data.Array
import Graphics.Vty
import Graphics.Vty.CrossPlatform

type Maze = Array (Int,Int) Char

playMaze :: Maze -> IO String
playMaze maze = bracket (mkVty defaultConfig) shutdown $ playMaze' maze

playMaze' :: Maze -> Vty -> IO String
playMaze' maze vty = eventLoop vty maze (1,0)

style :: Attr
style = defAttr `withForeColor` white `withBackColor` black

basePicture :: Maze -> Picture
basePicture maze =
  let ((ymin,xmin),(ymax,xmax)) = bounds maze
      mazeRows = [[maze ! (y,x) | x <- [xmin..xmax]] | y <- [ymin..ymax]]
      infoRow = "wasd or hjkl to move, q or ESC to quit"
  in picForImage $ foldr1 (<->) $ map (string style) (mazeRows ++ [infoRow])

eventLoop :: Vty -> Maze -> (Int,Int) -> IO String
eventLoop vty maze (y,x) = do
  let ((_,_),(ymax,xmax)) = bounds maze
  if y == ymax-1 && x == xmax
    then return "Congratulations! You won!\n"
    else do
    let playerImage = translate x y (char style '@')
        composition = addToTop (basePicture maze) playerImage
    update vty composition
    e <- nextEvent vty
    case e of
      EvKey (KChar 'q') [] -> return ""
      EvKey KEsc [] -> return ""
      EvKey (KChar 'h') [] -> goLeft vty maze (y,x)
      EvKey (KChar 'l') [] -> goRight vty maze (y,x)
      EvKey (KChar 'k') [] -> goUp vty maze (y,x)
      EvKey (KChar 'j') [] -> goDown vty maze (y,x)
      EvKey (KChar 'w') [] -> goUp vty maze (y,x)
      EvKey (KChar 'a') [] -> goLeft vty maze (y,x)
      EvKey (KChar 's') [] -> goDown vty maze (y,x)
      EvKey (KChar 'd') [] -> goRight vty maze (y,x)
      EvKey KUp    [] -> goUp vty maze (y,x)
      EvKey KLeft  [] -> goLeft vty maze (y,x)
      EvKey KDown  [] -> goDown vty maze (y,x)
      EvKey KRight [] -> goRight vty maze (y,x)
      _ -> eventLoop vty maze (y,x)

goLeft :: Vty -> Maze -> (Int,Int) -> IO String
goLeft vty maze (y,x) = do
  let ((_,xmin),(_,_)) = bounds maze
      x' = if x > xmin then x-1 else x
      c = maze ! (y,x')
  eventLoop vty maze $ if c == ' ' then (y,x') else (y,x)

goRight :: Vty -> Maze -> (Int,Int) -> IO String
goRight vty maze (y,x) = do
  let ((_,_),(_,xmax)) = bounds maze
      x' = if x < xmax then x+1 else x
      c = maze ! (y,x')
  eventLoop vty maze $ if c == ' ' then (y,x') else (y,x)

goUp :: Vty -> Maze -> (Int,Int) -> IO String
goUp vty maze (y,x) = do
  let ((ymin,_),(_,_)) = bounds maze
      y' = if y > ymin then y-1 else y
      c = maze ! (y',x)
  eventLoop vty maze $ if c == ' ' then (y',x) else (y,x)

goDown :: Vty -> Maze -> (Int,Int) -> IO String
goDown vty maze (y,x) = do
  let ((_,_),(ymax,_)) = bounds maze
      y' = if y < ymax then y+1 else y
      c = maze ! (y',x)
  eventLoop vty maze $ if c == ' ' then (y',x) else (y,x)
