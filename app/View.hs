module View where

import Graphics.Gloss
import Model
import Model.Entities
import Model.Menus
import Model.Settings
import Data.Map

window :: Display
window = InWindow "Pacman" (800, 600) (10, 10) 

pacmanClosed :: Picture
pacmanClosed = pictures [color yellow (circleSolid 80), translate 25 25 (color black (circleSolid 10))]

pacmanOpenMouth :: Float -> Picture
pacmanOpenMouth time = rotate 180 (pictures [color yellow (circleSolid 80), arcSolid (180 + 30 * abs(sin (3 * time))) (180 -  30 * abs(sin (3 * time))) 80])

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

type PacmanPicture = Picture
type BlinkyPicture = Picture
type InkyPicture = Picture
type PinkyPicture = Picture
type ClydePicture = Picture
  -- these get updated every frame
type SmallDotPicture = Picture
type PowerDotPicture = Picture
type CherryPicture = Picture
type StrawberryPicture = Picture
type OrangePicture = Picture
type ApplePicture = Picture
type MelonPicture = Picture
type GalaxianPicture = Picture
type BellPicture = Picture
type KeyPicture = Picture
type WallPicture = Picture
  -- these are constant every frame

data AnimatedSprites = MkASprites {
  pacmanSprite :: PacmanPicture,
  blinkySprite :: BlinkyPicture,
  inkySprite :: InkyPicture,
  pinkySprite :: PinkyPicture,
  clydeSprite :: ClydePicture
}

data ConstantSprites = MkCSprites{
    smallDotSprite :: SmallDotPicture,
    powerDotSprite :: PowerDotPicture,
    cherrySprite :: CherryPicture,
    strawberrySprite :: StrawberryPicture,
    orangeSprite :: OrangePicture,
    appleSprite :: ApplePicture,
    melonSprite :: MelonPicture,
    galaxianSprite :: GalaxianPicture,
    bellSprite :: BellPicture,
    keySprite :: KeyPicture
}
data Sprites = MkSprites {
  constantSprites :: ConstantSprites,
  animatedSprites :: AnimatedSprites
}

animateSprites :: GameState -> Float -> AnimatedSprites
animateSprites gamestate time = MkASprites 
  (animatePacman gamestate time)
  (animateBlinky gamestate time)
  (animateInky gamestate time)
  (animatePinky gamestate time)
  (animateClyde gamestate time)

animatePacman :: GameState -> Float -> PacmanPicture
animatePacman gs@(MkGameState _ _ _ _ (MkEntityRecord (MkPlayer _ dir _) _ _) _ _) time = rotate (playerToDir dir) (pictures [color yellow (circleSolid 80), arcSolid (180 + 30 * abs(sin (3 * time))) (180 -  30 * abs(sin (3 * time))) 80])

testGameState :: GameState
testGameState = MkGameState Playing Data.Map.empty 0 1 (MkEntityRecord (MkPlayer (MkPosition 0 0) South Weak) [] []) Neutral (Settings 0)

playerToDir :: Direction -> Float
playerToDir North = 90
playerToDir East  = 180
playerToDir South = 270
playerToDir West  = 0

animateBlinky :: GameState -> Float -> BlinkyPicture
animateBlinky = undefined

animateInky :: GameState -> Float -> InkyPicture
animateInky = undefined

animatePinky :: GameState -> Float -> PinkyPicture
animatePinky = undefined

animateClyde :: GameState -> Float -> ClydePicture
animateClyde = undefined

wallSprite :: Point -> WallPicture
wallSprite (x, y) = color blue (polygon [(x, y), (x + 5, y), (x, y + 5), (x + 5, y + 5)])

