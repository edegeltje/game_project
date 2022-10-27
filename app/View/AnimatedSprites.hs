module View.AnimatedSprites where

import Graphics.Gloss

import Model

import Model.Entities

type PacmanPicture = Picture
type BlinkyPicture = Picture
type InkyPicture = Picture
type PinkyPicture = Picture
type ClydePicture = Picture


data AnimatedSprites = MkASprites {
  pacmanSprite :: PacmanPicture,
  blinkySprite :: BlinkyPicture,
  inkySprite :: InkyPicture,
  pinkySprite :: PinkyPicture,
  clydeSprite :: ClydePicture
}


animateSprites :: GameState -> Float -> AnimatedSprites
animateSprites gamestate time = MkASprites 
  (animatePacman gamestate time)
  (animateBlinky gamestate time)
  (animateInky gamestate time)
  (animatePinky gamestate time)
  (animateClyde gamestate time)

animatePacman :: GameState -> Float -> PacmanPicture

animatePacman gamestate = pacmanOpenMouth where
  dir = getDirection $ player $ entities gamestate

openingtime = 0.40

pacmanOpenMouth :: Float -> PacmanPicture
pacmanOpenMouth time = color yellow (arcSolid halfAngle (-halfAngle) 80) where
  halfMaxAngle = 45
  halfAngle = halfMaxAngle * openPercent
  openPercent = abs(sin (time * pi / openingtime))

animateBlinky :: GameState -> Float -> BlinkyPicture
animateBlinky = undefined

animateInky :: GameState -> Float -> InkyPicture
animateInky = undefined

animatePinky :: GameState -> Float -> PinkyPicture
animatePinky = undefined

animateClyde :: GameState -> Float -> ClydePicture
animateClyde = undefined
