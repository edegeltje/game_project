module Model.Entities where

data Position = MkPosition {xposition:: Int, yposition::Int}
  deriving (Eq, Ord)

data Direction = North | South | East | West 
  deriving (Eq)

class Positioned a where
  getPosition :: a -> Position

data EntityHeap = MkEntityHeap {
  player :: PlayerEntity,
  enemies :: [EnemyEntity],
  dots :: [Dot]--,
--  fruits :: [Fruits]
}

data Dot = SmallDot Position | PowerDot Position

instance Positioned Dot where 
  getPosition (SmallDot a) = a
  getPosition (PowerDot a) = a

class (Positioned a) => Agent a where
  setPosition :: a -> Position -> a
  getDirection :: a -> Direction
  setDirection :: a -> Direction -> a

data PowerState = PoweredUp | Weak

data PlayerEntity = MkPlayer {
  playerPosition :: Position,
  playerMovementDirection :: Direction,
  powerState :: PowerState
}
instance Positioned PlayerEntity where
  getPosition player = playerPosition player

instance Agent PlayerEntity where
  setPosition player newPosition = player {playerPosition = newPosition}
  getDirection player = playerMovementDirection player
  setDirection player newDirection = player {playerMovementDirection = newDirection}

data EnemyMovementType = RedGhost | OrangeGhost | CyanGhost | PinkGhost

data EnemyStatus = Alive | Dead

data EnemyEntity = MkEnemy {
  enemyPosition :: Position,
  enemyMovementDirection :: Direction,
  enemyMovementType :: EnemyMovementType,
  enemyStatus :: EnemyStatus
}