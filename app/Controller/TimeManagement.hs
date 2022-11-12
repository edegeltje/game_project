module Controller.TimeManagement where

import Control.Monad.State.Lazy
import Model
import Model.Menus
import Model.Entities

updateTime ::Float -> State GameState Float
updateTime dt = do
  realtime <- getTime
  speed <- getGameSpeed
  let realdt = dt * speed
  animationAdvanced <- advanceAnimation realdt
  if not animationAdvanced 
    then do
      putTime (realtime + realdt)
      forEntities (updateEntitiesTime realdt)
      getTime
    else
      getTime

advanceAnimation :: Float -> State GameState Bool
advanceAnimation realdt = do
  gs <- get
  let aState = animationState gs
  case aState of
    NoAnimation -> return False
    GameOver t -> do
      put gs {animationState = GameOver (t-realdt)}
      return True
    WinScreen t | t-realdt <0 -> do
      put gs {animationState = NoAnimation, menuState= StartMenu LoadOption}
      return False
               | otherwise -> do
      put gs {animationState = WinScreen (t-realdt)}
      return True


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
    PoweredUp t -> PoweredUp (t-realdt)
    Weak -> Weak
    DeadIn t -> DeadIn (t-realdt))

updateEnemyTime :: Float -> State EnemyEntity ()
updateEnemyTime realdt = do
  es <- getEnemyStatus
  putEnemyStatus (case es of
    Dead t | t-realdt > 0 -> Dead (t-realdt)
           | otherwise -> Alive
    Scared -> Scared
    Alive -> Alive
    ScaredDeadIn t -> ScaredDeadIn (t-realdt))

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

checkTimers :: State GameState ()
checkTimers = do
  forEntities checkPowerStateTimer
  forEntities checkEnemyPatternTimer

checkEnemyPatternTimer :: State EntityRecord ()
checkEnemyPatternTimer = do
  return ()

checkPowerStateTimer :: State EntityRecord Bool
checkPowerStateTimer = do
  pstate <- forPlayer getPowerState
  case pstate of
    PoweredUp x | x < 0 -> do
      forPlayer $ putPowerState Weak
      return True
    _ -> return False

checkZombieGhosts :: State GameState ()
checkZombieGhosts = do
  enemies <- forEntities getEnemies
  newEnemies <- mapM checkZombieGhost enemies
  forEntities $ putEnemies newEnemies
  return ()

checkPlayerDeathTimer :: State GameState ()
checkPlayerDeathTimer = do
  player <- forEntities getPlayer
  case powerState player of
    DeadIn x | x < 0 -> do
      addScore (-1000)
      forEntities $ forPlayer $ putPowerState Weak
    _ -> return ()
    

hasBeenKilled :: EnemyEntity -> Bool
hasBeenKilled e = case enemyStatus e of
  Alive -> True
  Dead x -> True
  Scared -> False
  ScaredDeadIn x -> False

checkZombieGhost :: EnemyEntity -> State GameState EnemyEntity
checkZombieGhost e = do
  case enemyStatus e of
    Alive -> return e
    Dead x -> return e
    Scared -> return e
    ScaredDeadIn x | x < 0 -> do
      enemies <- forEntities getEnemies
      let killedEnemies = [enemy | enemy <- enemies, hasBeenKilled enemy]
      addScore $ 200 * 2 ^length killedEnemies
      return e {enemyStatus = Dead 60}
    _ -> return e



unScare :: State EntityRecord ()
unScare = do
  forAllEnemies $ do
    estatus <- getEnemyStatus
    case estatus of
      Scared -> putEnemyStatus Alive
      _ -> return ()
    dir <- getDirection
    putDirection $ oppositeDir dir
  return ()