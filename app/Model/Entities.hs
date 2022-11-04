module Model.Entities where
import Graphics.Gloss

type Position = (Int,Int)

toFloatTuple :: Position -> (Float,Float)
toFloatTuple (x,y) = (fromIntegral x, fromIntegral y)

data Direction = North | South | East | West 
  deriving (Eq, Show)

class Positioned a where
  getPosition :: a -> Position

class Positioned a => Renderable a where
  getSpriteIO :: a -> Float -> IO Picture

data EntityRecord = MkEntityRecord {
  player :: PlayerEntity,
  enemies :: [EnemyEntity],
  fruits :: [Fruit]
}

class (Positioned a) => Agent a where
  setPosition :: a -> Position -> a
  getDirection :: a -> Direction
  setDirection :: a -> Direction -> a

data PowerState = PoweredUp | Weak

data PlayerEntity = MkPlayer {
  playerPosition :: !Position,
  playerMovementDirection :: !Direction,
  powerState :: !PowerState
}
instance Positioned PlayerEntity where
  getPosition = playerPosition

instance Agent PlayerEntity where
  setPosition player newPosition = player {playerPosition = newPosition}
  getDirection = playerMovementDirection
  setDirection player newDirection = player {playerMovementDirection = newDirection}

data EnemyMovementType = Blinky | Inky | Pinky | Clyde

data EnemyStatus = Alive | Dead | Scared

data EnemyEntity = MkEnemy {
  enemyPosition :: !Position,
  enemyMovementDirection :: Direction,
  enemyMovementType :: EnemyMovementType,
  enemyStatus :: EnemyStatus
}

instance Positioned EnemyEntity where
  getPosition = enemyPosition

instance Agent EnemyEntity where
  setPosition enemy newPosition = enemy {enemyPosition = newPosition}
  getDirection = enemyMovementDirection
  setDirection enemy newDirection = enemy {enemyMovementDirection = newDirection}

data FruitType = Cherry | Strawberry | Orange | Apple | Melon | Galaxian | Bell | Key
data Fruit = MkFruit {
  fruitType :: FruitType,
  fruitPosition :: Position
}

instance Positioned Fruit where
  getPosition = fruitPosition