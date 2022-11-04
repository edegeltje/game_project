module Model where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
import qualified Data.Map as DM

import Model.Entities (EntityRecord (enemies, fruits), Position, EnemyEntity, Fruit)
import Model.Menus (MenuState)
import Model.Settings

foo :: IO ()
foo = putStrLn "foo function in Model"

data GameState = MkGameState {
  menuState :: MenuState,
  maze :: BottomLayer,
  score :: Score,
  level :: Int,
  entities :: EntityRecord,
  inputBuffer :: InputButton,
  time :: Float,
  settings :: Settings
  }

type Score = Int

type BottomLayer = DM.Map Position BottomLayerContent

data InputButton = InputNeutral | InputUp | InputDown | InputLeft | InputRight | InputSelect | InputBack

data BottomLayerContent = Wall | SmallDot | PowerDot | Empty
  deriving (Eq, Show)
{-
This datastructure to prevent non-accesible states from being represented
-}

type BottomLayer' = String
-- this is how we will represent the maze in the Json files 

data Level = MkLevel {
  levelNumber :: Int,
  levelMaze :: BottomLayer',
  levelEnemies :: [EnemyEntity],
  levelFruits :: [Fruit]
}
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