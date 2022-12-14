{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE DeriveGeneric #-}
module Model.Entities where
import Control.Monad.State (get,State, put, runState)
import System.Random
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
import GHC.Generics

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

addPos :: Position -> Position -> Position
addPos (x1,y1) (x2,y2) = (x1+x2,y1+y2)

intTimes :: Int -> Position -> Position
intTimes a (x,y) = (a*x,a*y)


floatTimes :: Float -> Position -> Point
floatTimes a pos = a A.* toFloatTuple pos

toFloatTuple :: Position -> (Float,Float)
toFloatTuple (x,y) = (fromIntegral x, fromIntegral y)

dist :: Position -> Position -> Int
dist (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2
data Direction = North | West | South | East
  deriving (Eq, Show, Ord, Generic)

dirToPos :: Direction -> Position
dirToPos North = (0,1)
dirToPos South = (0,-1)
dirToPos East = (1,0)
dirToPos West = (-1,0)

oppositeDir :: Direction -> Direction
oppositeDir dir = case dir of
  North -> South
  West -> East
  South -> North
  East -> West

class Positioned a where
  position :: a -> Position
  speed :: a -> Float
  setSpeed :: a -> Float -> a
  direction :: a -> Direction
  setDirection :: a -> Direction -> a

  getDirection :: State a Direction
  getDirection = do
    direction <$> get
  putDirection :: Direction -> State a ()
  putDirection dir = do
    a <- get
    put (setDirection a dir)
  
  getSpeed :: State a Float
  getSpeed = do
    speed <$> get
  
  putSpeed :: Float -> State a ()
  putSpeed s = do
    a <- get
    put (setSpeed a s)

coIncidesWith :: (Positioned a,Positioned b) => a -> b -> Bool
coIncidesWith a b = position a == position b

class Positioned a => Renderable a where
  getSprite ::  FruitSprites -> a -> Float -> Picture

data GhostMovementPattern = Scatter | Chase
  deriving (Show, Generic)
data EntityRecord = MkEntityRecord {
  player :: PlayerEntity,
  enemies :: [EnemyEntity],
  fruits :: [Fruit],
  enemyPattern :: GhostMovementPattern,
  patternSequence :: [Float]}
  deriving (Show, Generic)

getPlayer :: State EntityRecord PlayerEntity
getPlayer = do
  player <$> get
putPlayer :: PlayerEntity -> State EntityRecord ()
putPlayer p = do
  entities <- get
  put entities {player= p}

getEnemies :: State EntityRecord [EnemyEntity]
getEnemies = do
  enemies <$> get
putEnemies :: [EnemyEntity] -> State EntityRecord ()
putEnemies es = do
  entities <- get
  put entities {enemies = es}

getFruits :: State EntityRecord [Fruit]
getFruits = do
  fruits <$> get
putFruits :: [Fruit] -> State EntityRecord ()
putFruits fs = do
  entities <- get
  put entities {fruits = fs}

getGhostPattern :: State EntityRecord GhostMovementPattern
getGhostPattern = do
  enemyPattern <$> get
putGhostMovementPattern :: GhostMovementPattern -> State EntityRecord ()
putGhostMovementPattern p = do
  entities <- get
  put entities {enemyPattern = p }

forAllEnemies :: State EnemyEntity a -> State EntityRecord [a] -- some lifting functions
forAllEnemies action = do
  enemies <- getEnemies
  let result = map (runState action) enemies
  let state = map snd result
  putEnemies state
  return $ map fst result

forAllFruits :: State Fruit a -> State EntityRecord [a]
forAllFruits action = do
  fruits <- getFruits
  let result = map (runState action) fruits
  putFruits $ map snd result
  return $ map fst result

forPlayer :: State PlayerEntity a -> State EntityRecord a
forPlayer action = do
  player <- getPlayer
  let result = runState action player
  putPlayer $ snd result
  return $ fst result

class (Positioned a) => Agent a where
  setPosition :: a -> Position -> a

  getPosition :: State a Position
  getPosition = do
    position <$> get

  putPosition :: Position -> State a ()
  putPosition pos = do
    a <- get
    put (setPosition a pos)



data PowerState = PoweredUp Float | Weak | DeadIn Float
  deriving (Show, Generic)

data PlayerEntity = MkPlayer {
  playerPosition :: !Position,
  playerMovementDirection :: !Direction,
  powerState :: !PowerState,
  playerSpeed :: !Float}
  deriving (Show, Generic)

getPowerState :: State PlayerEntity PowerState
getPowerState = do
  powerState <$> get

putPowerState :: PowerState -> State PlayerEntity ()
putPowerState powers = do
  p <- get
  put p {powerState = powers}


instance Positioned PlayerEntity where
  position = playerPosition
  speed = playerSpeed
  setSpeed player newSpeed = player {playerSpeed = newSpeed}
  direction = playerMovementDirection
  setDirection p newDirection = p {playerMovementDirection = newDirection}


instance Agent PlayerEntity where
  setPosition p newPosition = p {playerPosition = newPosition}


data EnemyName = Blinky | Inky | Pinky | Clyde
  deriving (Show,Eq,Generic)
data EnemyStatus = Alive | Dead Float | Scared | ScaredDeadIn Float
  deriving (Show,Eq,Generic)
data EnemyEntity = MkEnemy {
  enemyPosition :: !Position,
  enemyTarget :: !Position,
  enemyMovementDirection :: !Direction,
  enemyMovementType :: EnemyName,
  enemyStatus :: EnemyStatus,
  enemySpeed :: !Float}
  deriving (Show, Generic, Eq)

getEnemyStatus :: State EnemyEntity EnemyStatus
getEnemyStatus = do
  enemyStatus <$> get
putEnemyStatus :: EnemyStatus -> State EnemyEntity ()
putEnemyStatus eSt = do
  e <- get
  put e {enemyStatus = eSt }

getEnemyTarget :: State EnemyEntity Position
getEnemyTarget = do
  enemyTarget <$> get

putEnemyTarget :: Position -> State EnemyEntity ()
putEnemyTarget t = do
  e <- get
  put e {enemyTarget = t}

instance Positioned EnemyEntity where
  position = enemyPosition
  speed = enemySpeed
  setSpeed enemy newSpeed = enemy {enemySpeed = newSpeed}
  direction = enemyMovementDirection
  setDirection enemy newDirection = enemy {enemyMovementDirection = newDirection}

instance Agent EnemyEntity where
  setPosition enemy newPosition = enemy {enemyPosition = newPosition}


data FruitType = Cherry | Strawberry | Orange | Apple | Melon | Galaxian | Bell | Key
  deriving (Show, Generic)
data Fruit = MkFruit {
  fruitType :: FruitType,
  fruitPosition :: Position,
  fruitTimer :: Float}
  deriving (Show, Generic)

getFruitType :: State Fruit FruitType
getFruitType = do
  fruitType <$> get

setFruitType :: FruitType -> State Fruit ()
setFruitType ft = do
  f <- get
  put (f {fruitType = ft})

getFruitTimer :: State Fruit Float
getFruitTimer = do
  fruitTimer <$> get

putFruitTimer :: Float -> State Fruit ()
putFruitTimer t = do
  f <- get
  put (f {fruitTimer = t})

instance Positioned Fruit where
  position = fruitPosition
  speed = const 0
  setSpeed = const
  direction = const East
  setDirection = const