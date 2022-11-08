module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Model
import Model.Entities
import Model.Menus
import Model.Settings
import qualified Data.Map as DM
import View.AnimatedSprites
import View.StaticSprites (window, fruitSpritesIO)
import Controller

import qualified View.ViewGame as VG
import qualified View.ViewMenu as VM

animatePacTest :: IO ()
animatePacTest = animate window black (animatePacman testPlayer)

test :: IO ()
test =  display (InWindow "example" (800, 600) (0, 0)) black (color green (circle 100))

baz :: IO()
baz = putStrLn "baz function in View"

view :: FruitSprites -> GameState -> Picture
view fs gs@MkGameState {menuState = Playing} = VG.view fs gs
view fs gs = pictures [VM.view gs
  ,color white $ translate' (-30,-10) $ scale 0.2 0.2 $ text $ show $ inputBuffer gs,
  color white $ translate' (-30,-30) $ scale 0.2 0.2 $ text $ show $ menuState gs
  ]


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
    []
    Chase)
  InputNeutral 
  0
  (MkSettings 0 0)


tuples = [(x,y) | x<- [-2..20], y <- [-2..20]]
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
  MkEnemy (2,1) (0,0) North Inky Alive,
  MkEnemy (2,3) (0,0) East Pinky Alive,
  MkEnemy (4,1) (0,0) West Blinky Alive,
  MkEnemy (4,3) (0,0) South Clyde Alive
  ]

testFruits = [
  MkFruit Cherry     (7,1)  60,
  MkFruit Bell       (7,3)  60,
  MkFruit Apple      (9,1)  60,
  MkFruit Galaxian   (9,3)  60,
  MkFruit Key        (11,1) 60,
  MkFruit Melon      (11,3) 60,
  MkFruit Orange     (13,1) 60,
  MkFruit Strawberry (13,3) 60]

testPlayer' :: PlayerEntity
testPlayer' = MkPlayer (15,1) West Weak

testEntities :: EntityRecord
testEntities = MkEntityRecord testPlayer' testEnemies testFruits Chase

testGameState' :: GameState
testGameState' = 
  MkGameState
    Playing testMaze 1 2 testEntities InputNeutral 0 (MkSettings 1 1)

calcGameState :: Float -> GameState
calcGameState t = MkGameState
    Playing testMaze 1 2 testEntities InputNeutral t (MkSettings 1 1)

testView = do
  fs <- fruitSpritesIO
  animateIO window black
    (return . view fs . calcGameState) (const $ return ())