{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Controller.GameControllerWState where

import Data.Maybe
import Control.Monad.State.Lazy
import Model
import Model.Menus
import Model.Entities
import Controller.GameController
import View.StaticSprites
import qualified Data.Map as DM
import Data.List (sort, sortOn)

getNewPacDirection :: State GameState Direction
getNewPacDirection = do
  ib <- getInputBuffer
  dir <- forEntities $ forPlayer getDirection
  return $ fromMaybe dir (inputToDir ib)

newAgentPosition:: Agent a => a-> State GameState a
newAgentPosition agent =do
  m <- getMaze
  let newPos = if dirPossible m pos dir
        then calcNewPosition pos dir
        else pos
      pos = position agent
      dir = direction agent
  return $ execState (putPosition newPos) agent

updatePacDirection :: State GameState ()
updatePacDirection = do
  pos <- forEntities $ forPlayer getPosition
  maze <- getMaze
  newDir <- getNewPacDirection

  if dirPossible maze pos newDir
    then forEntities $ forPlayer $ putDirection newDir
    else return ()

collectCollectibles :: State GameState ()
collectCollectibles = do
  player <- forEntities getPlayer
  pos <- forEntities (forPlayer getPosition)
  maze <- getMaze
  case getPositionContent pos maze of
    Wall -> return ()
    SmallDot ->
      addScore 10
    PowerDot -> do
      addScore 50
      powerUp
    Empty -> return ()
  putMaze $ DM.insert pos Empty maze
  eatFruit
  if not (any (\x -> x== SmallDot || x == PowerDot ) maze)
    then levelComplete
    else return ()

levelComplete = undefined

eatFruit :: State GameState ()
eatFruit = do
  player <- forEntities getPlayer
  fruits <- forEntities getFruits
  addScore $ case [fruitType f | f<- fruits, f `coIncidesWith` player] of
    [Cherry]     -> 100
    [Strawberry] -> 300
    [Orange]     -> 500
    [Apple]      -> 700
    [Melon]      -> 1000
    [Galaxian]   -> 2000
    [Bell]       -> 3000
    [Key]        -> 5000
    _            -> 0
  forEntities $ putFruits [f |f <- fruits, not (f `coIncidesWith` player)]

powerUp :: State GameState ()
powerUp = do
  entities <- getEntities
  putEntities (flip execState entities $ do
    powerUpPacman
    scareGhosts)

powerUpPacman :: State EntityRecord ()
powerUpPacman = do
  forPlayer $ putPowerState (PoweredUp 10)

scareGhosts :: State EntityRecord ()
scareGhosts = () <$ forAllEnemies scareGhost -- ignore the result from the mapping
-- (it has type [()], so very useful, i know), return unit instead

scareGhost :: State EnemyEntity ()
scareGhost = do
  status <-getEnemyStatus
  case status of
    Alive -> putEnemyStatus Scared
    _ -> return ()

stepbutwithstate:: Float -> GameState -> IO GameState
stepbutwithstate t =return . execState (statefulStep t)

statefulStep :: Float -> State GameState ()
statefulStep t = do
  previoustime <- getTime
  newtime <- updateTime t -- increase all timers, return the new time
  if floor newtime >floor previoustime -- when the seconds change, 
                                       --take an iterationstep
    then iterationStep
    else return ()


iterationStep :: State GameState ()
iterationStep = do
  movePlayer
  collectCollectibles
  moveEnemies
  hedies <- killOrBeKilled
  if hedies then heDies else heLives

heLives :: State GameState ()
heLives = do
  updatePacDirection

wankyDirToPos :: Direction -> Position
wankyDirToPos dir = case dir of
  North -> (-1,1)
  dir -> dirToPos dir

calculateGhostDirection ::BottomLayer -> EnemyEntity -> Direction
calculateGhostDirection maze enemy = do
  let currentDir = direction enemy
      currentPos = position enemy
      currentTar = enemyTarget enemy
      directions = [dir | dir <- [North,South,East,West], dir /= currentDir]
      possibleDirections = [dir | dir <- directions, dirPossible maze currentPos dir]
      sortedPDirs = sort possibleDirections
      betterSortedDirs = sortOn (dist currentTar . dirToPos) sortedPDirs
    
    in case betterSortedDirs of
      [] -> currentDir
      a:_ -> a
    
killOrBeKilled :: State GameState Bool
killOrBeKilled = do
  p <- forEntities getPlayer
  es <- forEntities getEnemies
  let qualifyingEnemies = [enemy | enemy <- es, p `coIncidesWith` enemy]
      scaredEs = [enemy | enemy <- qualifyingEnemies, enemyStatus enemy == Scared]
      aliveEs = [enemy | enemy <- qualifyingEnemies, enemyStatus enemy == Alive]
      numberKilledGhosts = length [enemy | enemy <- es, enemyStatus enemy /= Scared]
  case aliveEs of
    e:es -> return True -- pacmanDies = true
    _ -> do
      addScore (200 * 2 ^ numberKilledGhosts)
      forEntities $ killQualifiedGhosts (\e -> p `coIncidesWith` e && enemyStatus e == Scared)
      return False -- pacmanDies = false

  undefined
heDies :: State GameState ()
heDies = do
  addScore (-1000)
  --maybe add something else here too? idk


killQualifiedGhosts :: (EnemyEntity -> Bool) -> State EntityRecord ()
killQualifiedGhosts q = do
  enemies <- getEnemies
  forAllEnemies (do
    e<- get
    if q e then putEnemyStatus (Dead 60) else return ())
  return ()

movePlayer :: State GameState ()
movePlayer = do
  p <- forEntities getPlayer
  newp <- newAgentPosition p
  forEntities (putPlayer newp)

moveEnemies :: State GameState ()
moveEnemies = do
  enemies <- forEntities getEnemies
  newenemies <- mapM newAgentPosition enemies
  forEntities (putEnemies newenemies)

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

coIncidesWith :: (Positioned a,Positioned b) => a -> b -> Bool
coIncidesWith a b = position a == position b

addScore :: Score -> State GameState ()
addScore ds= do
  score <- getScore
  putScore (score+ds)
