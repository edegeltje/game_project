module View where

import Graphics.Gloss
import Model
import Model.Entities
import Model.Menus
import Model.Settings
import Data.Map
import View.AnimatedSprites
import View.StaticSprites

window :: Display
window = InWindow "Pacman" (800, 600) (10, 10) 

animatePacTest :: IO ()
animatePacTest = animate window black (animatePacman testGameState)

test :: IO ()
test =  display (InWindow "example" (800, 600) (0, 0)) black (color green (circle 100))

baz :: IO()
baz = putStrLn "baz function in View"

view :: GameState -> IO Picture
view = do
  undefined
  

-- the following types are Aliases to prevent boolean blindness.

  -- these get updated every frame

  -- these are constant every frame


data Sprites = MkSprites {
  constantSprites :: ConstantSprites,
  animatedSprites :: AnimatedSprites
}

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
  (Settings 0)

