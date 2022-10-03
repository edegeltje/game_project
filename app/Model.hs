module Model where

import Data.Map

import Model.Entities (EntityHeap, Position)
import Model.Menus (MenuState)

foo :: IO ()
foo = putStrLn "foo function in Model"

data GameState = Gamestate {
  menustate :: MenuState,
  maze :: Maze,
  score :: Score,
  entities :: EntityHeap
  }

type Score = Int

type Maze = Map Position TileContent

data ContainsEnemy = DoesContainEnemy | DoesntContainEnemy 

data ContainsPlayerOrCollectible = DoesContainPlayer 
                                 | DoesContainCollectible
                                 | DoesntContainEither

data TileContent = Wall | NotWall (ContainsEnemy, ContainsPlayerOrCollectible)
{-
This datastructure to prevent non-accesible states from being represented
-}
