module Model.Entities where
import Graphics.Gloss

type SmallDotPicture = Picture
type PowerDotPicture = Picture
type CherryPicture = Picture
type StrawberryPicture = Picture
type OrangePicture = Picture
type ApplePicture = Picture
type MelonPicture = Picture
type GalaxianPicture = Picture
type BellPicture = Picture
type KeyPicture = Picture
type WallPicture = Picture

data FruitSprites = MkFSprites{
    appleSprite :: ApplePicture,
    bellSprite :: BellPicture,
    cherrySprite :: CherryPicture,
    galaxianSprite :: GalaxianPicture,
    keySprite :: KeyPicture,
    melonSprite :: MelonPicture,
    orangeSprite :: OrangePicture,
    strawberrySprite :: StrawberryPicture
    }
  deriving Show

type Position = (Int,Int)

toFloatTuple :: Position -> (Float,Float)
toFloatTuple (x,y) = (fromIntegral x, fromIntegral y)

data Direction = North | South | East | West 
  deriving (Eq, Show)

class Positioned a where
  getPosition :: a -> Position

class Positioned a => Renderable a where
  getSprite ::  FruitSprites -> a -> Float -> Picture

data EntityRecord = MkEntityRecord {
  player :: PlayerEntity,
  enemies :: [EnemyEntity],
  fruits :: [Fruit]}
  deriving Show

class (Positioned a) => Agent a where
  setPosition :: a -> Position -> a
  getDirection :: a -> Direction
  setDirection :: a -> Direction -> a

data PowerState = PoweredUp | Weak
  deriving Show

data PlayerEntity = MkPlayer {
  playerPosition :: !Position,
  playerMovementDirection :: !Direction,
  powerState :: !PowerState}
  deriving Show

instance Positioned PlayerEntity where
  getPosition = playerPosition

instance Agent PlayerEntity where
  setPosition player newPosition = player {playerPosition = newPosition}
  getDirection = playerMovementDirection
  setDirection player newDirection = player {playerMovementDirection = newDirection}

data EnemyMovementType = Blinky | Inky | Pinky | Clyde
  deriving Show
data EnemyStatus = Alive | Dead | Scared
  deriving Show
data EnemyEntity = MkEnemy {
  enemyPosition :: !Position,
  enemyMovementDirection :: Direction,
  enemyMovementType :: EnemyMovementType,
  enemyStatus :: EnemyStatus}
  deriving Show

instance Positioned EnemyEntity where
  getPosition = enemyPosition

instance Agent EnemyEntity where
  setPosition enemy newPosition = enemy {enemyPosition = newPosition}
  getDirection = enemyMovementDirection
  setDirection enemy newDirection = enemy {enemyMovementDirection = newDirection}

data FruitType = Cherry | Strawberry | Orange | Apple | Melon | Galaxian | Bell | Key
  deriving Show
data Fruit = MkFruit {
  fruitType :: FruitType,
  fruitPosition :: Position}
  deriving Show

instance Positioned Fruit where
  getPosition = fruitPosition