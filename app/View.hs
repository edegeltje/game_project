module View where

import Graphics.Gloss
import Model
import Model.Entities
import Model.Menus
import Model.Settings
import Data.Map
import View.AnimatedSprites
import View.StaticSprites ( WallPicture, ConstantSprites, testIOsprite)

window :: Display
window = InWindow "Pacman" (800, 600) (10, 10) 

animatePacTest :: IO ()
animatePacTest = animate window black pacmanOpenMouth

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

dirToAngle :: Direction -> Float
dirToAngle North = 90
dirToAngle East  = 0
dirToAngle South = 270
dirToAngle West  = 180

rotateSpriteToDir :: Direction -> Picture -> Picture
rotateSpriteToDir = rotate . dirToAngle

testGameState :: GameState
testGameState = MkGameState 
  Playing 
  Data.Map.empty 
  0 
  1 
  (MkEntityRecord 
    (MkPlayer 
      (MkPosition 0 0) 
      South 
      Weak)
    []
    []) 
  Neutral 
  (MkSettings 0 0)

wallSprite' :: Point -> WallPicture
wallSprite' (x, y) = color blue (polygon [(x, y), (x + 5, y), (x, y + 5), (x + 5, y + 5)])

