{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Controller.JsonInteract where

import qualified Data.Map as DM

import Data.Aeson

import Model 
import Model.Entities
import Model.Settings
import Model.Menus
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import View


instance FromJSON Level where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON BottomLayerContent where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Fruit where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON FruitType where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON EnemyEntity where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON EnemyName where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON EnemyStatus where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Direction where
  parseJSON = genericParseJSON defaultOptions


instance ToJSON Level where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON BottomLayerContent where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Fruit where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON FruitType where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EnemyEntity where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EnemyStatus where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EnemyName where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Direction where
  toEncoding = genericToEncoding defaultOptions

gameStateToLevel :: GameState -> Level
gameStateToLevel gs = MkLevel (level gs) (maze gs) (enemies $ entities gs) (fruits $ entities gs)

levelToGameState :: Level -> GameState
levelToGameState lvl = MkGameState 
  Playing 
  (levelMaze lvl) 
  0 
  (levelNumber lvl) 
  (levelEntities lvl) 
  InputNeutral 
  0 
  (MkSettings 1 1)
  testRngSeed

levelEntities :: Level -> EntityRecord
levelEntities lvl = MkEntityRecord standardPlayer (levelEnemies lvl) (levelFruits lvl) Chase

standardPlayer :: PlayerEntity
standardPlayer = MkPlayer (15,1) West Weak 1

saveLevel :: IO ()
saveLevel = B.writeFile "levels/level1.json" (encode (gameStateToLevel level1GameState))

loadLevel :: Int -> IO Level
loadLevel i = do
  inputFile <- B.readFile ("levels/level" ++ show i ++ ".json")
  let parsedFile = decode inputFile
  return (fromJust parsedFile)

-- saveGameState :: GameState -> IO ()
-- saveGameState savedState = B.writeFile "savedState.json" (encode savedState)



--applyLevel :: GameState -> Level -> GameState
--applyLevel gs level =
--  gs {
--    level = levelNumber level,
--    maze = jsonMazeToMaze $ levelMaze level,
--    entities = (entities gs) {
--      enemies = levelEnemies level,
--      fruits = levelFruits level}}

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
addIntToKey y = DM.mapKeys $ \x -> (x,y)

linesToContentMap :: [String] -> BottomLayer
linesToContentMap lines = DM.unions $ zipWith addIntToKey [0..] lineMaps
  where
    lineMaps = map lineToContentMap lines

