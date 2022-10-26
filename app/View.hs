module View where

import Graphics.Gloss
import Model

window :: Display
window = InWindow "Pacman" (800, 600) (10, 10) 

pacmanClosed :: Picture
pacmanClosed = pictures [color yellow (circleSolid 80), translate 25 25 (color black (circleSolid 10))]

pacmanOpenMouth :: Float -> Picture
pacmanOpenMouth time = rotate 180 (pictures [color yellow (circleSolid 80), arcSolid (-abs(sin time)) (abs(sin time)) 80])

animatePacTest :: IO ()
animatePacTest = animate window black pacmanOpenMouth

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

animateSprites :: GameState -> AnimatedSprites
animateSprites gamestate = MkASprites 
  (animatePacman gamestate)
  (animateBlinky gamestate)
  (animateInky gamestate)
  (animatePinky gamestate)
  (animateClyde gamestate)


animatePacman :: GameState -> PacmanPicture
animatePacman = undefined

animateBlinky :: GameState -> BlinkyPicture
animateBlinky = undefined

animateInky :: GameState -> InkyPicture
animateInky = undefined

animatePinky :: GameState -> PinkyPicture
animatePinky = undefined

animateClyde :: GameState -> ClydePicture
animateClyde = undefined

