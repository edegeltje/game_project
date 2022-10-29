module Model where

import Data.Map

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
  settings :: Settings
  }

type Score = Int

type BottomLayer = Map Position BottomLayerContent

data InputButton = InputNeutral | InputUp | InputDown | InputLeft | InputRight | InputSelect | InputBack

data BottomLayerContent = Wall | SmallDot | PowerDot | Empty
  deriving Eq
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