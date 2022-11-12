{-# LANGUAGE DeriveGeneric #-}
module Model where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
import qualified Data.Map as DM
import Control.Monad.State.Lazy
import Control.Monad
import System.Random

import Model.Entities (EntityRecord (enemies, fruits), Position, EnemyEntity, Fruit)
import Model.Menus (MenuState)
import Model.Settings
import GHC.Generics
import Data.Aeson


foo :: IO ()
foo = putStrLn "foo function in Model"

data GameState = MkGameState {
  menuState :: MenuState,
  maze :: !BottomLayer,
  score :: !Score,
  level :: !Int,
  entities :: !EntityRecord,
  inputBuffer :: !InputButton,
  time :: !Float,
  settings :: !Settings,
  rngStuff :: !RngStuff
  }
  deriving (Show, Generic)
type Score = Int

data RngStuff = MkRngStuff {
  rngState :: !StdGen,
  rngConstruct :: RngConstruct
}

instance Show RngStuff where
  show a = show (rngConstruct a)

data RngConstruct = MkRngConst {
  rngCount :: !Int,
  rngSeed :: !Int
}
  deriving (Show, Generic)

initialiseRngState :: RngConstruct -> (RngStuff,Int)
initialiseRngState const = (
  MkRngStuff (mkStdGen $ rngSeed const) const {rngCount = 0},
  rngCount const)

hydrateRngStuff :: (RngStuff, Int) -> RngStuff
hydrateRngStuff (rng,num)= execState (replicateM num (selectRandom [1..4])) rng

addScore :: Score -> State GameState ()
addScore ds= do
  score <- getScore
  putScore (score+ds)

forEntities :: State EntityRecord a -> State GameState a

forEntities action = do
  entities <- getEntities
  let result = runState action entities
  putEntities $ snd result
  return $ fst result

getSettings :: State GameState Settings
getSettings = do
  settings <$> get

getGameSpeed :: State GameState Float
getGameSpeed = do
  gameSpeed <$> getSettings


getInputBuffer :: State GameState InputButton
getInputBuffer = do
  inputBuffer <$> get
getMaze :: State GameState BottomLayer
getMaze = do
  maze <$> get

putMaze :: BottomLayer -> State GameState ()
putMaze m = do
  gs <- get
  put gs {maze=m}

getEntities :: State GameState EntityRecord
getEntities = do
  entities <$> get

putEntities :: EntityRecord -> State GameState ()
putEntities es = do
  gs <- get
  put gs {entities = es}

getScore :: State GameState Score
getScore = do
  score <$> get
putScore :: Score -> State GameState ()
putScore s = do
  gs <- get
  put gs {score = s}



getTime :: State GameState Float
getTime = do
  time <$> get
putTime :: Float -> State GameState ()
putTime t = do
  gs <- get
  put gs {time = t}

getRngState :: State RngStuff StdGen
getRngState = do
  rngState <$> get
putRngState :: StdGen -> State RngStuff ()
putRngState rng = do
  rngstuff <- get
  put rngstuff {rngState = rng}

getRngStuff :: State GameState RngStuff
getRngStuff = do
  rngStuff <$> get
putRngStuff :: RngStuff -> State GameState ()
putRngStuff rngstuff = do
  gs <- get
  put gs {rngStuff = rngstuff}

forRngStuff :: State RngStuff a -> State GameState a
forRngStuff action = do
  rngstuff <- getRngStuff
  let result = runState action rngstuff
  putRngStuff $ snd result
  return $ fst result

getRngConst :: State RngStuff RngConstruct
getRngConst = do
  rngConstruct <$> get

putRngConst :: RngConstruct -> State RngStuff ()
putRngConst a = do
  stuff <- get
  put stuff {rngConstruct = a}

forRngConst :: State RngConstruct a -> State RngStuff a
forRngConst action = do
  rngConst <- getRngConst
  let result = runState action rngConst
  putRngConst $ snd result
  return $ fst result

upRngCounter :: State RngStuff ()
upRngCounter = do
  rngstuff@MkRngStuff {rngConstruct=rngConst@MkRngConst {rngCount=count}} <- get
  put $ rngstuff {rngConstruct = rngConst {rngCount = count + 1}}



selectRandom :: [a] -> State RngStuff a
selectRandom as = do
  rng <- getRngState
  let (roll,newRng) = uniformR (0,length as-1) rng
  putRngState newRng
  upRngCounter
  return $ as !! roll

type BottomLayer = DM.Map Position BottomLayerContent

data InputButton = InputNeutral | InputUp | InputDown | InputLeft | InputRight | InputSelect | InputBack
  deriving (Show, Generic)
data BottomLayerContent = Wall | SmallDot | PowerDot | Empty
  deriving (Eq, Show, Generic)
{-
This datastructure to prevent non-accesible states from being represented
-}

type BottomLayer' = String
-- this is how we will represent the maze in the Json files 

data Level = MkLevel {
  levelNumber :: !Int,
  levelMaze :: !BottomLayer,
  levelEnemies :: ![EnemyEntity],
  levelFruits :: [Fruit]
} deriving (Show, Generic)
-- the type for the levels we store in the json files

tileToPoint :: (Float, Float) -> (Float,Float)
tileToPoint = (tILESIZE A.*)

translate' :: (Float,Float) -> Picture -> Picture
translate' = uncurry translate . tileToPoint

polygon' :: [(Float, Float)] -> Picture
polygon' = polygon . map tileToPoint
circleSolid' :: Float -> Picture
circleSolid' c= circleSolid $ tILESIZE * c
arcSolid' :: Float -> Float -> Float -> Picture
arcSolid' a b c = arcSolid a b $ tILESIZE * c


picturesIO :: [IO Picture] -> IO Picture
picturesIO =  (pictures <$>) . sequence


magicNumber :: Int
magicNumber = 42069

