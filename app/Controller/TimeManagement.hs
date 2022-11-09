module Controller.TimeManagement where

import Control.Monad.State.Lazy
import Model
import Model.Entities

updateTime ::Float -> State GameState Float
updateTime dt = do
  realtime <- getTime
  speed <- getGameSpeed
  let realdt = dt * speed
  putTime (realtime + realdt)
  forEntities (updateEntitiesTime realdt)
  getTime

updateEntitiesTime :: Float -> State EntityRecord ()
updateEntitiesTime realdt = do
  forPlayer $ updatePlayerTime realdt
  forAllEnemies $ updateEnemyTime realdt
  forAllFruits $ updateFruitTime realdt
  yeetBadFruits
  return ()

updatePlayerTime :: Float -> State PlayerEntity ()
updatePlayerTime realdt = do
  ps <- getPowerState
  putPowerState (case ps of
    PoweredUp t
      |t - realdt > 0 -> PoweredUp (t-realdt)
      |otherwise -> Weak
    Weak -> Weak)

updateEnemyTime :: Float -> State EnemyEntity ()
updateEnemyTime realdt = do
  es <- getEnemyStatus
  putEnemyStatus (case es of
    Dead t | t-realdt > 0 -> Dead (t-realdt)
           | otherwise -> Alive
    Scared -> Scared
    Alive -> Alive)

updateFruitTime :: Float -> State Fruit ()
updateFruitTime realdt = do
  ft <- getFruitTimer
  putFruitTimer (ft-realdt)

goodFruit :: Fruit -> Bool
goodFruit f = 0 <= fruitTimer f

yeetBadFruits :: State EntityRecord ()
yeetBadFruits = do
  fruits <- getFruits
  putFruits [f | f<- fruits, goodFruit f]


checkEnemyPatternTimer :: State EntityRecord ()
checkEnemyPatternTimer = do
  return ()

checkPowerStateTimer :: State EntityRecord Bool
checkPowerStateTimer = do
  pstate <- forPlayer getPowerState  
  case pstate of
    PoweredUp x | x < 0 -> do
      return True
    _ -> return False

unScare :: State EntityRecord ()
unScare = do
  forAllEnemies $ do
    estatus <- getEnemyStatus
    case estatus of
      Scared -> putEnemyStatus Alive
      _ -> return ()
    reverseDirection
  return ()

reverseDirection :: Agent a => State a ()

reverseDirection = do
  dir <- getDirection
  putDirection $ oppositeDir dir