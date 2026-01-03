module PlayMaze (playMaze, Maze) where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Graphics.Vty
import Graphics.Vty.CrossPlatform

type Maze = Array (Int,Int) Char
 -- state is player's (y,x)
type Op = MaybeT (StateT (Int,Int) (ReaderT (Vty,Maze) IO))

playMaze :: Maze -> IO (Maybe String)
playMaze maze = bracket (mkVty defaultConfig) shutdown $ playMaze' maze

playMaze' :: Maze -> Vty -> IO (Maybe String)
playMaze' maze vty = runReaderT (evalStateT (runMaybeT eventLoop) (1,0)) (vty,maze)

getVty :: Op Vty
getVty = fst <$> ask

getMaze :: Op Maze
getMaze = snd <$> ask

getBounds :: Op ((Int,Int),(Int,Int))
getBounds = bounds <$> getMaze

getUL :: Op (Int,Int)
getUL = fst <$> getBounds

getYMin :: Op Int
getYMin = fst <$> getUL

getXMin :: Op Int
getXMin = snd <$> getUL

getBR :: Op (Int,Int)
getBR = snd <$> getBounds

getYMax :: Op Int
getYMax = fst <$> getBR

getXMax :: Op Int
getXMax = snd <$> getBR

style :: Attr
style = defAttr `withForeColor` white `withBackColor` black

basePicture :: Op Picture
basePicture = do
  maze <- getMaze
  ((ymin,xmin),(ymax,xmax)) <- getBounds
  let rows = [[maze ! (y,x) | x <- [xmin..xmax]] | y <- [ymin..ymax]] ++
        ["arrows, wasd, or hjkl to move, q or ESC to quit"]
  return $ picForImage $ foldr1 (<->) $ map (string style) rows

playerImage :: Op Image
playerImage = do
  (y,x) <- get
  return $ translate x y $ char style '@'

drawScreen :: Op ()
drawScreen = do
  base <- basePicture
  player <- playerImage
  vty <- getVty
  liftIO $ update vty $ addToTop base player

getEvent :: Op Event
getEvent = do
  vty <- getVty
  liftIO $ nextEvent vty

checkForWin :: Op Bool
checkForWin = do
  (y,x) <- get
  (ymax,xmax) <- getBR
  if y == ymax-1 && x == xmax
    then return True
    else return False

goLeft :: Op ()
goLeft = do
  maze <- getMaze
  xmin <- getXMin
  (y,x) <- get
  let x' = if x > xmin then x-1 else x
      c = maze ! (y,x')
  put $ if c == ' ' then (y,x') else (y,x)

goRight :: Op ()
goRight = do
  maze <- getMaze
  xmax <- getXMax
  (y,x) <- get
  let x' = if x < xmax then x+1 else x
      c = maze ! (y,x')
  put $ if c == ' ' then (y,x') else (y,x)

goUp :: Op ()
goUp = do
  maze <- getMaze
  ymin <- getYMin
  (y,x) <- get
  let y' = if y > ymin then y-1 else y
      c = maze ! (y',x)
  put $ if c == ' ' then (y',x) else (y,x)

goDown :: Op ()
goDown = do
  maze <- getMaze
  ymax <- getYMax
  (y,x) <- get
  let y' = if y < ymax then y+1 else y
      c = maze ! (y',x)
  put $ if c == ' ' then (y',x) else (y,x)

eventLoop :: Op String
eventLoop = do
  drawScreen
  e <- getEvent
  case e of
    EvKey (KChar 'q') [] -> mzero
    EvKey KEsc [] -> mzero
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
    _ -> return ()
  w <- checkForWin
  if w then return "Congratulations! You won!" else eventLoop
