module PlayMaze (playMaze) where

import Control.Exception (bracket)
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Graphics.Vty
import Graphics.Vty.CrossPlatform

type Maze = Array (Int,Int) Char
type Op = StateT (Int,Int) (ReaderT (Vty,Maze) IO) -- state is player's (y,x)

playMaze :: Maze -> IO String
playMaze maze = bracket (mkVty defaultConfig) shutdown $ playMaze' maze

playMaze' :: Maze -> Vty -> IO String
playMaze' maze vty = runReaderT (evalStateT eventLoop (1,0)) (vty,maze)

getVty :: Op Vty
getVty = fst <$> ask

getMaze :: Op Maze
getMaze = snd <$> ask

style :: Attr
style = defAttr `withForeColor` white `withBackColor` black

basePicture :: Maze -> Picture
basePicture maze =
  let ((ymin,xmin),(ymax,xmax)) = bounds maze
      mazeRows = [[maze ! (y,x) | x <- [xmin..xmax]] | y <- [ymin..ymax]]
      infoRow = "wasd or hjkl to move, q or ESC to quit"
  in picForImage $ foldr1 (<->) $ map (string style) (mazeRows ++ [infoRow])

eventLoop :: Op String
eventLoop = do
  maze <- getMaze
  (y,x) <- get
  let ((_,_),(ymax,xmax)) = bounds maze
  if y == ymax-1 && x == xmax
    then return "Congratulations! You won!\n"
    else do
    let playerImage = translate x y (char style '@')
        composition = addToTop (basePicture maze) playerImage
    vty <- getVty
    liftIO $ update vty composition
    e <- liftIO $ nextEvent vty
    case e of
      EvKey (KChar 'q') [] -> return ""
      EvKey KEsc [] -> return ""
      EvKey (KChar 'h') [] -> goLeft
      EvKey (KChar 'l') [] -> goRight
      EvKey (KChar 'k') [] -> goUp
      EvKey (KChar 'j') [] -> goDown
      EvKey (KChar 'w') [] -> goUp
      EvKey (KChar 'a') [] -> goLeft
      EvKey (KChar 's') [] -> goDown
      EvKey (KChar 'd') [] -> goRight
      EvKey KUp    [] -> goUp
      EvKey KLeft  [] -> goLeft
      EvKey KDown  [] -> goDown
      EvKey KRight [] -> goRight
      _ -> eventLoop

goLeft :: Op String
goLeft = do
  maze <- getMaze
  (y,x) <- get
  let ((_,xmin),(_,_)) = bounds maze
      x' = if x > xmin then x-1 else x
      c = maze ! (y,x')
  put $ if c == ' ' then (y,x') else (y,x)
  eventLoop

goRight :: Op String
goRight = do
  maze <- getMaze
  (y,x) <- get
  let ((_,_),(_,xmax)) = bounds maze
      x' = if x < xmax then x+1 else x
      c = maze ! (y,x')
  put $ if c == ' ' then (y,x') else (y,x)
  eventLoop

goUp :: Op String
goUp = do
  maze <- getMaze
  (y,x) <- get
  let ((ymin,_),(_,_)) = bounds maze
      y' = if y > ymin then y-1 else y
      c = maze ! (y',x)
  put $ if c == ' ' then (y',x) else (y,x)
  eventLoop

goDown :: Op String
goDown = do
  maze <- getMaze
  (y,x) <- get
  let ((_,_),(ymax,_)) = bounds maze
      y' = if y < ymax then y+1 else y
      c = maze ! (y',x)
  put $ if c == ' ' then (y',x) else (y,x)
  eventLoop
