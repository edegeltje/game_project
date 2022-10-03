module Model where

import Data.Map

foo :: IO ()
foo = putStrLn "foo function in Model"

data GameState = Gamestate {
    menustate :: MenuState,
    maze :: Maze,
    score :: Score
    }

data MenuState = PauseMenu | StartMenu | Playing

type Score = Int

type Maze = Map Location (Maybe Entity)

data Location = MkLocation {xposition:: Int,yposition::Int}
  deriving (Eq, Ord)

data Direction = North | South | East | West 

data Entity = Agent | Collectible | Wall

class Positioned a where
  getPosition :: a -> Location
  setPosition :: a -> Location -> a

class (Positioned a) => Agent a where
  getDirection :: a -> Direction
  setDirection :: a -> Direction -> a



data PowerState = PoweredUp | Weak

data PlayerEntity = MkPlayer {
  location :: Location,
  movementDirection :: Direction,
  powerState :: PowerState
}



data EnemyEntity = MkEnemy {

}