module View.AnimatedSprites where

import Graphics.Gloss

import Data.Fixed

import Model

import Model.Entities
import View.StaticSprites (window)

type PacmanPicture = Picture
type GhostPicture = Picture

data AnimatedSprites = MkASprites {
  pacmanSprite :: PacmanPicture,
  ghostSprites :: [GhostPicture]
}


animateSprites :: GameState -> Float -> AnimatedSprites
animateSprites gameState time = MkASprites (animatePacman player' time)
  (map (($ time) . animateGhost) ghosts) where
    player' = player $ entities gameState
    ghosts = enemies $ entities gameState

animatePacman :: PlayerEntity -> Float -> PacmanPicture

animatePacman player = rotate (dirToAngle dir) . pacmanOpenMouth where
  dir = getDirection player

openingtime = 0.40

pacmanOpenMouth :: Float -> PacmanPicture
pacmanOpenMouth time = color yellow (arcSolid halfAngle (-halfAngle) 80) where
  halfMaxAngle = 45
  halfAngle = halfMaxAngle * openPercent
  openPercent = abs(sin (time * pi / openingtime))

-- shit het wil niet zoals ik wil.
-- voor een goede animate hebben we het volgende nodig:

animateGhost :: EnemyEntity -> Float -> GhostPicture
animateGhost ghost time = pictures [
  color ghostColor ghostTop,
  color ghostColor ghostBottom,
  translate 40 80 eye,
  translate 120 80 eye] where
    ghostColor = case enemyMovementType ghost of
      Blinky -> red
      Inky   -> blue
      Pinky  -> rose
      Clyde  -> orange
    dir = getDirection ghost
    ghostBottom = if mod' (time/openingtime) 1 > 0.5 then ghostBottom1 else ghostBottom2
    eye = dirToEye dir

dirToAngle :: Direction -> Float
dirToAngle North = 270
dirToAngle East  = 180
dirToAngle South = 90
dirToAngle West  = 0

ghostTop = pictures [polygon [(0,0), (160, 0), (160, 80), (0, 80)], 
  translate 80 80 (arcSolid 0 180 80)]

ghostBottom1 = pictures [arcSolid 270 360 40,
  translate 80 0 (arcSolid 180 0 40),
  translate 160 0 (arcSolid 180 270 40)]

ghostBottom2 = pictures [translate 40 0 (arcSolid 180 0 40),
  translate 120 0 (arcSolid 180 0 40)]

dirToCoords :: Direction -> (Float, Float) -- not quite a position 
-- (points are different from vectors)
dirToCoords North = (0,1)
dirToCoords East  = (1,0)
dirToCoords South = (0,-1)
dirToCoords West  = (-1,0)

irisOffset :: Float
irisOffset = 15
eyeWhiteOffset :: Float
eyeWhiteOffset = 10

dirToEye :: Direction -> Picture
dirToEye dir = pictures [color white $
  translate xEyeWhiteOffset yEyeWhiteOffset $ circleSolid 20,
  color blue $ translate xIrisOffset yIrisOffset $ circleSolid 8 ] where
    (xmult, ymult) = dirToCoords dir
    xIrisOffset = xmult * irisOffset
    yIrisOffset = ymult * irisOffset + northCorrection
    xEyeWhiteOffset = xmult * eyeWhiteOffset
    yEyeWhiteOffset = ymult * eyeWhiteOffset + northCorrection
    northCorrection = if dir == North then 30 else 0

testGhost = MkEnemy
  (MkPosition 0 0)
  South
  Clyde
  Alive

renderTestGhost = animate window black $ animateGhost testGhost