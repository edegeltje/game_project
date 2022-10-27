module View where

import Graphics.Gloss
import Model
import Model.Entities
import Model.Menus
import Model.Settings
import Data.Map
import View.AnimatedSprites
import View.StaticSprites ( WallPicture, ConstantSprites, testSpriteIO, window)


animatePacTest :: IO ()
animatePacTest = animate window black (animatePacman testPlayer)

test :: IO ()
test =  display (InWindow "example" (800, 600) (0, 0)) black (color green (circle 100))

baz :: IO()
baz = putStrLn "baz function in View"

view :: GameState -> IO Picture
view = do
  undefined
  

data Sprites = MkSprites {
  constantSprites :: ConstantSprites,
  animatedSprites :: AnimatedSprites
}

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
  Neutral 
  (MkSettings 0 0)

