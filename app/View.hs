module View where

import Graphics.Gloss
import Model
import Model.Entities
import Model.Menus
import Model.Settings
import Data.Map
import View.AnimatedSprites
import View.StaticSprites ( WallPicture, testSpriteIO, window)

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
      (MkPosition 0 0) 
      South 
      Weak

testGameState :: GameState
testGameState = MkGameState 
  Playing 
  Data.Map.empty 
  0 
  1 
  (MkEntityRecord 
    testPlayer
    []
    []) 
  InputNeutral 
  0
  (MkSettings 0 0)

