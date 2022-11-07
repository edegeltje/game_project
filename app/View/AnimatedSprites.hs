module View.AnimatedSprites where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
import Data.Fixed

import Model

import Model.Entities
import View.StaticSprites (window)
import Model.Settings

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

instance Renderable PlayerEntity where
  getSprite = const animatePacman

animatePacman :: PlayerEntity -> Float -> PacmanPicture

animatePacman player = rotate (dirToAngle dir) . pacmanOpenMouth where
  dir = getDirection player

openingtime = 1

pacmanOpenMouth :: Float -> PacmanPicture
pacmanOpenMouth time = color yellow (arcSolid' halfAngle (-halfAngle) 1) where
  halfMaxAngle = 45
  halfAngle = halfMaxAngle * openPercent
  openPercent = abs(sin (time * pi / openingtime))

-- shit het wil niet zoals ik wil.
-- voor een goede animate hebben we het volgende nodig:

animateGhost :: EnemyEntity -> Float -> GhostPicture
animateGhost ghost@MkEnemy{enemyStatus = Alive} time = pictures [
  color ghostColor ghostTop,
  color ghostColor ghostBottom,
  translate' (0.5, 1)  eye,
  translate' (1.5, 1) eye] where
    ghostColor = case enemyMovementType ghost of
      Blinky -> red
      Inky   -> blue
      Pinky  -> rose
      Clyde  -> orange
    dir = getDirection ghost
    ghostBottom = if mod' (time/openingtime) 1 > 0.5 then ghostBottom1 else ghostBottom2
    eye = dirToEye dir
animateGhost ghost@MkEnemy{enemyStatus = Scared} time = pictures [
  color (mixColors 0.7 0.3 blue black) ghostTop,
  color (mixColors 0.7 0.3 blue black) ghostBottom,
  translate' (0.5, 1)  eye,
  translate' (1.5, 1) eye] where
    ghostBottom = if mod' (time/openingtime) 1 > 0.5 then ghostBottom1 else ghostBottom2
    eye = color red (circleSolid' 0.25)

instance Renderable EnemyEntity where
  getSprite = const animateGhost

dirToAngle :: Direction -> Float
dirToAngle North = 270
dirToAngle East  = 0
dirToAngle South = 90
dirToAngle West  = 180

ghostTop = pictures [
  polygon' [(0,0),
    (2, 0),
    (2, 1),
    (0, 1)],
  translate' (1,1) (arcSolid' 0 180 1)]

ghostBottom1 = pictures [arcSolid' 270 360 0.5,
  translate' (1,0) (arcSolid' 180 0 0.5),
  translate' (2,0) (arcSolid' 180 270 0.5)]

ghostBottom2 = pictures [
  translate' (0.5,0) (arcSolid' 180 0 0.5),
  translate' (1.5, 0) (arcSolid' 180 0 0.5)]

dirToCoords :: Direction -> (Float, Float) -- not quite a position 
-- (points are different from vectors)
dirToCoords North = (0,1)
dirToCoords East  = (1,0)
dirToCoords South = (0,-1)
dirToCoords West  = (-1,0)

irisOffset :: Float
irisOffset = 0.2
eyeWhiteOffset :: Float
eyeWhiteOffset = 0.1

dirToEye :: Direction -> Picture
dirToEye dir = translate' northCorrection $ pictures [
  color white $ 
    translate' (eyeWhiteOffset A.* dirCoords) $ circleSolid' 0.25,
  color blue $ 
    translate' (irisOffset A.* dirCoords ) $ circleSolid' 0.1 ] where
      dirCoords = dirToCoords dir
      northCorrection = if dir == North then (0,0.4) else (0,0)

testGhost = MkEnemy
  (0,0)
  South
  Clyde
  Alive

renderTestGhost = animate window black $ animateGhost testGhost