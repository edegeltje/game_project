module Controller.JsonInteract where

import qualified Data.Map as DM

import Data.Aeson

import Model 
import Model.Entities

loadLevel :: Int -> IO Level
loadLevel = undefined

applyLevel :: GameState -> Level -> GameState
applyLevel gs level =
  gs {
    level = levelNumber level,
    maze = jsonMazeToMaze $ levelMaze level,
    entities = (entities gs) {
      enemies = levelEnemies level,
      fruits = levelFruits level}}

jsonMazeToMaze :: BottomLayer' -> BottomLayer
jsonMazeToMaze = linesToContentMap . lines


charToContent :: Char -> BottomLayerContent
charToContent c = case c of
  'w' -> Wall
  '.' -> SmallDot
  'O' -> PowerDot
  _ -> Empty

lineToContentMap :: String -> DM.Map Int BottomLayerContent
lineToContentMap line = DM.fromList $ zip [0..] $ map charToContent line

addIntToKey :: Int -> DM.Map Int BottomLayerContent -> BottomLayer
addIntToKey y = DM.mapKeys $ flip MkPosition y

linesToContentMap :: [String] -> BottomLayer
linesToContentMap lines = DM.unions $ zipWith addIntToKey [0..] lineMaps
  where
    lineMaps = map lineToContentMap lines

