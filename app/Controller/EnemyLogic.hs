module Controller.EnemyLogic where

import Model
import Model.Entities
import Control.Monad.State.Lazy
import qualified Data.Map as DM
import Controller.MoveEntities
import Data.List

-- the original pac-man has a bit janky logic. 
-- see https://www.youtube.com/watch?v=ataGotQ7ir8
-- in order to mimic this behaviour, we use wankyDirToPos

wankyDirToPos :: Direction -> Position
wankyDirToPos dir = case dir of
  North -> (-1,1)
  dir -> dirToPos dir

deadHome :: Position 
deadHome = (10,10)
clydeHome :: Position
clydeHome = (0,0)
blinkyHome :: Position
blinkyHome = (20,20)
inkyHome :: Position
inkyHome = (20,0)
pinkyHome :: Position
pinkyHome = (0,20)

calculateBlinkyChaseTarget :: GameState -> Position
calculateBlinkyChaseTarget = position . player . entities
calculateBlinkyScatterTarget :: GameState -> Position
calculateBlinkyScatterTarget gs = if dotsLeft < 50
  then calculateBlinkyChaseTarget gs
  else blinkyHome
  where
    dotsLeft = DM.size $ DM.filter (\x -> x== SmallDot || x == PowerDot ) $ maze gs

calculatePinkyChaseTarget :: GameState -> Position
calculatePinkyChaseTarget gs = pos `addPos` (4 `intTimes` wankyDirToPos dir)
  where
    p = player $ entities gs
    pos = position p
    dir = direction p
calculatePinkyScatterTarget :: GameState -> Position
calculatePinkyScatterTarget = const pinkyHome

calculateInkyChaseTarget :: GameState -> Position
calculateInkyChaseTarget gs = (2 `intTimes` jppos) `addPos` ((-1) `intTimes` bPos)
  where
    p = player $ entities gs
    playerPos = position p
    dir = direction p
    jppos = playerPos `addPos` (2 `intTimes` wankyDirToPos dir) --janky player position
    bPos = case [ghost | ghost <- enemies $ entities gs, enemyMovementType ghost == Blinky] of
      [] -> inkyHome
      a:_ -> position a
calculateInkyScatterTarget :: GameState -> Position
calculateInkyScatterTarget = const inkyHome

calculateClydeChaseTarget :: GameState -> Position
calculateClydeChaseTarget gs = if dist ppos cpos >64
  then ppos
  else clydeHome
    where
      ppos = position $ player $ entities gs
      cpos = case [ghost | ghost <- enemies $ entities gs, enemyMovementType ghost == Clyde] of
        [] -> clydeHome
        a:_ -> position a
calculateClydeScatterTarget :: GameState -> Position
calculateClydeScatterTarget = const clydeHome

calculateTargetDirection ::BottomLayer -> EnemyEntity -> Direction
calculateTargetDirection maze enemy = do
  let currentDir = direction enemy
      currentPos = position enemy
      currentTar = enemyTarget enemy
      directions = [dir | dir <- [North,South,East,West], dir /= oppositeDir currentDir]
      possibleDirections = [dir | dir <- directions, dirPossible maze currentPos dir]
      sortedPDirs = sort possibleDirections
      betterSortedDirs = sortOn (dist currentTar . addPos currentPos . dirToPos) sortedPDirs

    in case betterSortedDirs of
      [] -> oppositeDir currentDir
      a:_ -> a


setEnemyTarget :: EnemyEntity -> State GameState EnemyEntity
setEnemyTarget e = do
  case enemyStatus e of
    Dead x -> return e {enemyTarget = deadHome}
    _ -> do
      movementPattern <- forEntities getGhostPattern
      maze <- getMaze
      tgt <- (case (enemyMovementType e, movementPattern) of
        (Blinky, Scatter) -> calculateBlinkyScatterTarget
        (Blinky, Chase)   -> calculateBlinkyChaseTarget
        (Inky, Scatter)   -> calculateInkyScatterTarget
        (Inky, Chase)     -> calculateInkyScatterTarget
        (Pinky, Scatter)  -> calculatePinkyScatterTarget
        (Pinky, Chase)    -> calculatePinkyChaseTarget
        (Clyde, Scatter)  -> calculateClydeScatterTarget
        (Clyde, Chase)    -> calculateClydeChaseTarget) <$> get
      return e {enemyTarget = tgt}

setEnemyDirection :: EnemyEntity -> State GameState EnemyEntity
setEnemyDirection e = do
  maze <- getMaze
  case enemyStatus e of
    Scared -> do
      dir <- selectRandom [ dir|dir <- [North,East,South,West], dirPossible maze (position e) dir]
      return $ setDirection e dir
    _ -> return $ setDirection e $ calculateTargetDirection maze e
    
setEnemyDirections :: State GameState ()
setEnemyDirections = do
  enemies <- forEntities getEnemies
  updatedEnemies <- mapM (setEnemyDirection >=> setEnemyTarget) enemies
  -- >=> :: Monad m => (a-> m b) -> (b -> m c) (a -> m c)
  -- basically, (.) but with MOAR MONAD
  forEntities $ putEnemies updatedEnemies

