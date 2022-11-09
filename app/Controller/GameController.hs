{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Controller.GameController where

import Data.Maybe
import Control.Monad.State.Lazy
import Model
import Model.Entities

import Controller.MoveEntities

import Controller.TimeManagement

import View.StaticSprites
import qualified Data.Map as DM
import Data.List (sort, sortOn)
import Controller.EnemyLogic

step :: Float -> GameState -> IO GameState
step =stepbutwithstate

inputFromButton :: GameState -> InputButton -> IO GameState
inputFromButton gs ib = return gs {inputBuffer = ib}

inputToDir :: InputButton -> Maybe Direction
inputToDir InputUp = Just North
inputToDir InputRight = Just East
inputToDir InputLeft = Just West
inputToDir InputDown = Just South
inputToDir _ = Nothing

getNewPacDirection :: State GameState Direction
getNewPacDirection = do
  ib <- getInputBuffer
  dir <- forEntities $ forPlayer getDirection
  return $ fromMaybe dir (inputToDir ib)


updatePacDirection :: State GameState ()
updatePacDirection = do
  pos <- forEntities $ forPlayer getPosition
  maze <- getMaze
  newDir <- getNewPacDirection

  if dirPossible maze pos newDir
    then do
      forEntities $ forPlayer $ putDirection newDir
      forEntities $ forPlayer $ putSpeed 1
    else do
      oldDir <- forEntities $ forPlayer getDirection
      if not (dirPossible maze pos oldDir)
        then forEntities $ forPlayer $ putSpeed 0
        else return ()


stepbutwithstate:: Float -> GameState -> IO GameState
stepbutwithstate t =return . execState (statefulStep t)

statefulStep :: Float -> State GameState ()
statefulStep t = do
  previoustime <- getTime
  newtime <- updateTime t -- increase all timers, return the new time
  checkPlayerDeathTimer
  checkZombieGhosts
  if floor newtime /= floor previoustime -- when the seconds change, 
    then simulationStep                   -- take an iterationstep.
    else return ()



simulationStep :: State GameState ()
simulationStep = do
  movePlayer
  moveEnemies
  poweredUp <- collectCollectibles
  hedies <- killOrBeKilled
  if hedies then heDies
    else return ()
  heLives poweredUp
  killOrBeKilledHalfway


heLives :: Bool -> State GameState ()
heLives poweredUp= do
  updatePacDirection
  setEnemyDirections poweredUp
  return ()

heDies :: State GameState ()
heDies = do
  addScore (-1000)
  --maybe add something else here too? idk

heDiesHalfway :: State GameState ()
heDiesHalfway = do
  forEntities $ forPlayer $ putPowerState $ DeadIn 0.5

killOrBeKilledHalfway :: State GameState ()
killOrBeKilledHalfway = do
  player <- forEntities getPlayer
  if speed player /= 0 -- if the speed is zero, it will collide next step anyway
  then do 
    es <- forEntities getEnemies
    nextPosition <- position <$> newAgentPosition player
    let collidingEnemies = [e | e <- es,
          direction e == oppositeDir (direction player), 
          position e == nextPosition]
        killableEnemies = [e | e <- collidingEnemies, enemyStatus e == Scared]
        killingEnemies = [e | e <- collidingEnemies, enemyStatus e == Alive]
    case killingEnemies of
      [] -> return ()
      e:_ -> heDiesHalfway
    forEntities $ killQualifiedGhosts (`elem` killableEnemies)
  else
    return ()
