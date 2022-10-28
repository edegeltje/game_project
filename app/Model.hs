module Model where

import Data.Map

import Model.Entities (EntityRecord, Position)
import Model.Menus (MenuState)
import Model.Settings

foo :: IO ()
foo = putStrLn "foo function in Model"

data GameState = MkGameState {
  menustate :: MenuState,
  maze :: BottomLayer,
  score :: Score,
  level :: Int,
  entities :: EntityRecord,
  inputBuffer :: InputBuffer,
  settings :: Settings
  }

type Score = Int

type BottomLayer = Map Position BottomLayerContent

data InputBuffer = Neutral | Up | Down | Left | Right

data BottomLayerContent = Wall | SmallDot | PowerDot | Empty
  deriving Eq
{-
This datastructure to prevent non-accesible states from being represented
-}
