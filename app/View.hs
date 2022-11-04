module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Model
import Model.Entities
import Model.Menus
import Model.Settings
import qualified Data.Map as DM
import View.AnimatedSprites
import View.StaticSprites ( WallPicture, window)

import qualified View.ViewGame as VG
import qualified View.ViewMenu as VM

animatePacTest :: IO ()
animatePacTest = animate window black (animatePacman testPlayer)

test :: IO ()
test =  display (InWindow "example" (800, 600) (0, 0)) black (color green (circle 100))

baz :: IO()
baz = putStrLn "baz function in View"

view :: GameState -> IO Picture
view gs@MkGameState {menuState = Playing} = VG.view gs
view gs = VM.view gs


testPlayer = MkPlayer 
      (0,0) 
      South 
      Weak

testGameState :: GameState
testGameState = MkGameState 
  Playing 
  DM.empty 
  0 
  1 
  (MkEntityRecord 
    testPlayer
    []
    []) 
  InputNeutral 
  0
  (MkSettings 0 0)


tuples = [(x,y) | x<- [-2..20], y <- [-220]]
contentCalc :: (Int,Int) -> BottomLayerContent
contentCalc (-2,_)  = Wall
contentCalc (20,_) = Wall
contentCalc (_,-2)  = Wall
contentCalc (_,20) = Wall
contentCalc (_,1)  = SmallDot
contentCalc (_,2)  = PowerDot
contentCalc _      = Empty
content = map contentCalc tuples

testMaze = DM.fromList $ zip tuples content :: BottomLayer

testEnemies = [
  MkEnemy (2,1) North Inky Alive,
  MkEnemy (2,3) East Pinky Alive,
  MkEnemy (4,1) West Blinky Alive,
  MkEnemy (4,3) South Clyde Alive
  ]

testFruits = [
  MkFruit Cherry     (7,1),
  MkFruit Bell       (7,3),
  MkFruit Apple      (9,1),
  MkFruit Galaxian   (9,3),
  MkFruit Key        (11,1),
  MkFruit Melon      (11,3),
  MkFruit Orange     (13,1),
  MkFruit Strawberry (13,3)]
testPlayer' = MkPlayer (15,1) West Weak

testEntities = MkEntityRecord testPlayer' testEnemies testFruits
testGameState' = 
  MkGameState
    Playing testMaze 1 2 testEntities InputNeutral 0 (MkSettings 1 1)
calcGameState t = MkGameState
    Playing testMaze 1 2 testEntities InputNeutral t (MkSettings 1 1)

testView = animateIO window black (view . calcGameState) controllerSetRedraw