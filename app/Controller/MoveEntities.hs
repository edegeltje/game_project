{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" "Use void" #-}
module Controller.MoveEntities where

import Model
import Model.Entities
import Control.Monad.State.Lazy
import qualified Data.Map as DM
import View.StaticSprites

dirPossible :: BottomLayer -> Position -> Direction -> Bool
dirPossible maze pos dir = getPositionContent (calcNewPosition pos dir) maze /= Wall

calcNewPosition :: Position -> Direction -> Position
calcNewPosition (x,y) = addPos (x,y) . dirToPos

newAgentPosition:: Agent a => a-> State GameState a
newAgentPosition agent =do
  m <- getMaze
  let newPos = if dirPossible m pos dir
        then calcNewPosition pos dir
        else pos
      pos = position agent
      dir = direction agent
  return $ execState (putPosition newPos) agent

movePlayer :: State GameState ()
movePlayer = do
  p <- forEntities getPlayer
  newp <- newAgentPosition p
  forEntities (putPlayer newp)


collectCollectibles :: State GameState Bool
collectCollectibles = do
  pos <- forEntities (forPlayer getPosition)
  maze <- getMaze
  powerUp <- case getPositionContent pos maze of
    Wall -> return False
    SmallDot -> do
      addScore 10
      return False
    PowerDot -> do
      addScore 50
      forEntities powerUpPacman
      return True
    Empty -> return False
  putMaze $ DM.insert pos Empty maze
  eatFruit
  if not (any (\x -> x== SmallDot || x == PowerDot ) maze)
    then levelComplete
    else return ()
  return powerUp


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


powerUpPacman :: State EntityRecord ()
powerUpPacman = do
  forPlayer $ putPowerState (PoweredUp 30)

scareGhosts :: State EntityRecord ()
scareGhosts = () <$ forAllEnemies scareGhost -- ignore the result from the mapping
-- (it has type [()], so very useful, i know), return unit instead

scareGhost :: State EnemyEntity ()
scareGhost = do
  status <-getEnemyStatus
  case status of
    Alive -> putEnemyStatus Scared
    _ -> return ()
  dir <- getDirection
  putDirection $ oppositeDir dir


moveEnemies :: State GameState ()
moveEnemies = do
  enemies <- forEntities getEnemies
  newenemies <- mapM newAgentPosition enemies
  forEntities (putEnemies newenemies)

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
      addScore $ 200 * sum (zipWith (\ x y -> 2 ^ x) [numberKilledGhosts..] scaredEs)
      forEntities $ killQualifiedGhosts (\e -> p `coIncidesWith` e && enemyStatus e == Scared)
      return False -- pacmanDies = false

putQualifiedGhostStatus :: EnemyStatus -> (EnemyEntity -> Bool) -> State EntityRecord ()
putQualifiedGhostStatus estatus q = do
  enemies <- getEnemies
  forAllEnemies (do
    e <- get
    if q e then putEnemyStatus estatus else return ())
  return ()


killQualifiedGhosts :: (EnemyEntity -> Bool) -> State EntityRecord ()
killQualifiedGhosts q = putQualifiedGhostStatus (Dead 60) q

