module PlayMaze (playMaze, MazeArray) where

import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Graphics.Vty
import Graphics.Vty.CrossPlatform

type MazeArray = Array (Int,Int) Char

data Environment = Environment
  { _maze :: Maze
  , _vty :: Vty
  }

data Maze = Maze
  { _xMin :: Int
  , _yMin :: Int
  , _xMax :: Int
  , _yMax :: Int
  , _chars :: MazeArray
  }

makeEnvironment :: MazeArray -> Vty -> Environment
makeEnvironment m v =
  let ((ymin_,xmin_),(ymax_,xmax_)) = bounds m
  in Environment (Maze xmin_ ymin_ xmax_ ymax_ m) v

$(makeLenses ''Environment)
$(makeLenses ''Maze)

data GameState = GameState
  { _basePicture :: Picture
  , _playerLoc :: Location
  }

data Location = Location
  { _locX :: Int
  , _locY :: Int
  }

$(makeLenses ''GameState)
$(makeLenses ''Location)

type Op = MaybeT (StateT GameState (ReaderT Environment IO))

startState :: GameState
startState = GameState bot (Location 0 1)
  where
    bot = bot

playMaze :: MazeArray -> IO (Maybe String)
playMaze m = bracket (mkVty defaultConfig) shutdown $ playMaze' m

playMaze' :: MazeArray -> Vty -> IO (Maybe String)
playMaze' m v = runReaderT (evalStateT (runMaybeT startPlaying) startState)
  (makeEnvironment m v)

startPlaying :: Op String
startPlaying = do
  makeBasePicture
  eventLoop

style :: Attr
style = defAttr `withForeColor` white `withBackColor` black

makeBasePicture :: Op ()
makeBasePicture = do
  xmin <- view (maze . xMin)
  ymin <- view (maze . yMin)
  xmax <- view (maze . xMax)
  ymax <- view (maze . yMax)
  cs <- view (maze . chars)
  let rows = [[cs ! (y,x) | x <- [xmin..xmax]] | y <- [ymin..ymax]] ++
        ["arrows, wasd, or hjkl to move, q or ESC to quit"]
  basePicture .= (picForImage (foldr1 (<->) $ map (string style) rows))
    { picBackground = Background ' ' style }

playerStyle :: Attr
playerStyle = defAttr `withForeColor` black `withBackColor` white

playerImage :: Op Image
playerImage = do
  x <- use (playerLoc . locX)
  y <- use (playerLoc . locY)
  return $ translate x y $ char playerStyle '@'

drawScreen :: Op ()
drawScreen = do
  base <- use basePicture
  player <- playerImage
  v <- view vty
  liftIO $ update v $ addToTop base player

getEvent :: Op Event
getEvent = do
  v <- view vty
  liftIO $ nextEvent v

checkForWin :: Op Bool
checkForWin = do
  x <- use (playerLoc . locX)
  y <- use (playerLoc . locY)
  xmax <- view (maze . xMax)
  ymax <- view (maze . yMax)
  if x == xmax && y == ymax-1
    then return True
    else return False

goLeft :: Op ()
goLeft = do
  x <- use (playerLoc . locX)
  y <- use (playerLoc . locY)
  xmin <- view (maze . xMin)
  cs <- view (maze . chars)
  let x' = if x > xmin then x-1 else x
      c = cs ! (y,x')
  playerLoc . locX .= if c == ' ' then x' else x

goRight :: Op ()
goRight = do
  x <- use (playerLoc . locX)
  y <- use (playerLoc . locY)
  xmax <- view (maze . xMax)
  cs <- view (maze . chars)
  let x' = if x < xmax then x+1 else x
      c = cs ! (y,x')
  playerLoc . locX .= if c == ' ' then x' else x

goUp :: Op ()
goUp = do
  x <- use (playerLoc . locX)
  y <- use (playerLoc . locY)
  ymin <- view (maze . yMin)
  cs <- view (maze . chars)
  let y' = if y > ymin then y-1 else y
      c = cs ! (y',x)
  playerLoc . locY .= if c == ' ' then y' else y

goDown :: Op ()
goDown = do
  x <- use (playerLoc . locX)
  y <- use (playerLoc . locY)
  ymax <- view (maze . yMax)
  cs <- view (maze . chars)
  let y' = if y < ymax then y+1 else y
      c = cs ! (y',x)
  playerLoc . locY .= if c == ' ' then y' else y

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
