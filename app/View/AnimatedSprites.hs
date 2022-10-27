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

animatePacman gamestate = rotate (dirToAngle dir) . pacmanOpenMouth where
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

dirToAngle :: Direction -> Float
dirToAngle North = 270
dirToAngle East  = 180
dirToAngle South = 90
dirToAngle West  = 0

blinky1 :: BlinkyPicture
blinky1 = pictures [color red 
                      (pictures [
                          arcSolid 270 0 40,
                          translate 80 0 (arcSolid 180 0 40),
                          translate 160 0 (arcSolid 180 270 40),
                          polygon [(0,0), (160, 0), (160, 80), (0, 80)], 
                          translate 80 80 (arcSolid 0 180 80)]), 
                       color white 
                       (pictures [
                          translate 40 80 (circleSolid 20), 
                          translate 120 80 (circleSolid 20)]), 
                       color blue 
                       (pictures [
                          translate 50 82.5 (circleSolid 5), 
                          translate 130 82.5 (circleSolid 5)])]

blinky2 :: BlinkyPicture
blinky2 = pictures [color red 
                      (pictures [
                          translate 40 0 (arcSolid 180 0 40),
                          translate 120 0 (arcSolid 180 0 40),
                          polygon [(0,0), (160, 0), (160, 80), (0, 80)], 
                          translate 80 80 (arcSolid 0 180 80)]), 
                       color white 
                       (pictures [
                          translate 40 80 (circleSolid 20), 
                          translate 120 80 (circleSolid 20)]), 
                       color blue 
                       (pictures [
                          translate 50 82.5 (circleSolid 5), 
                          translate 130 82.5 (circleSolid 5)])]

inky1 :: InkyPicture
inky1 = pictures [color blue 
                      (pictures [
                          arcSolid 270 0 40,
                          translate 80 0 (arcSolid 180 0 40),
                          translate 160 0 (arcSolid 180 270 40),
                          polygon [(0,0), (160, 0), (160, 80), (0, 80)], 
                          translate 80 80 (arcSolid 0 180 80)]), 
                       color white 
                       (pictures [
                          translate 40 80 (circleSolid 20), 
                          translate 120 80 (circleSolid 20)]), 
                       color blue 
                       (pictures [
                          translate 50 82.5 (circleSolid 5), 
                          translate 130 82.5 (circleSolid 5)])]

inky2 :: InkyPicture
inky2 = pictures [color blue 
                      (pictures [
                          translate 40 0 (arcSolid 180 0 40),
                          translate 120 0 (arcSolid 180 0 40),
                          polygon [(0,0), (160, 0), (160, 80), (0, 80)], 
                          translate 80 80 (arcSolid 0 180 80)]), 
                       color white 
                       (pictures [
                          translate 40 80 (circleSolid 20), 
                          translate 120 80 (circleSolid 20)]), 
                       color blue 
                       (pictures [
                          translate 50 82.5 (circleSolid 5), 
                          translate 130 82.5 (circleSolid 5)])]

pinky1 :: PinkyPicture
pinky1 = pictures [color rose
                      (pictures [
                          arcSolid 270 0 40,
                          translate 80 0 (arcSolid 180 0 40),
                          translate 160 0 (arcSolid 180 270 40),
                          polygon [(0,0), (160, 0), (160, 80), (0, 80)], 
                          translate 80 80 (arcSolid 0 180 80)]), 
                       color white 
                       (pictures [
                          translate 40 80 (circleSolid 20), 
                          translate 120 80 (circleSolid 20)]), 
                       color blue 
                       (pictures [
                          translate 50 82.5 (circleSolid 5), 
                          translate 130 82.5 (circleSolid 5)])]

pinky2 :: PinkyPicture
pinky2 = pictures [color rose
                      (pictures [
                          translate 40 0 (arcSolid 180 0 40),
                          translate 120 0 (arcSolid 180 0 40),
                          polygon [(0,0), (160, 0), (160, 80), (0, 80)], 
                          translate 80 80 (arcSolid 0 180 80)]), 
                       color white 
                       (pictures [
                          translate 40 80 (circleSolid 20), 
                          translate 120 80 (circleSolid 20)]), 
                       color blue 
                       (pictures [
                          translate 50 82.5 (circleSolid 5), 
                          translate 130 82.5 (circleSolid 5)])]

clyde1 :: ClydePicture
clyde1 = pictures [color orange 
                      (pictures [
                          arcSolid 270 0 40,
                          translate 80 0 (arcSolid 180 0 40),
                          translate 160 0 (arcSolid 180 270 40),
                          polygon [(0,0), (160, 0), (160, 80), (0, 80)], 
                          translate 80 80 (arcSolid 0 180 80)]), 
                       color white 
                       (pictures [
                          translate 40 80 (circleSolid 20), 
                          translate 120 80 (circleSolid 20)]), 
                       color blue 
                       (pictures [
                          translate 50 82.5 (circleSolid 5), 
                          translate 130 82.5 (circleSolid 5)])]

clyde2 :: ClydePicture
clyde2 = pictures [color orange 
                      (pictures [
                          translate 40 0 (arcSolid 180 0 40),
                          translate 120 0 (arcSolid 180 0 40),
                          polygon [(0,0), (160, 0), (160, 80), (0, 80)], 
                          translate 80 80 (arcSolid 0 180 80)]), 
                       color white 
                       (pictures [
                          translate 40 80 (circleSolid 20), 
                          translate 120 80 (circleSolid 20)]), 
                       color blue 
                       (pictures [
                          translate 50 82.5 (circleSolid 5), 
                          translate 130 82.5 (circleSolid 5)])]