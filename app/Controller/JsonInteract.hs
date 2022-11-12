{-# LANGUAGE OverloadedStrings #-}
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

instance FromJSON Settings where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON InputButton where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON EntityRecord where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON MenuState where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON StartMenuState where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON PauseMenuState where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON SettingMenuState where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON GhostMovementPattern where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON PlayerEntity where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON PowerState where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON AnimationState where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON RngConstruct where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON RngStuff where
  parseJSON = withObject "RngStuff" $ \v -> 
    let rngconst = v .: "RngConst" in
      hydrateRngStuff . initialiseRngState <$> rngconst 
      -- reminder: <$> :: (a->b) -> f a -> f b
      --           <*> :: f (a -> b) -> f a -> f b
      -- the type of the result in this case is Maybe RngStuff

instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \v ->
    MkGameState <$> 
      v .: "menuState" <*>
      v .: "maze" <*>
      v .: "score" <*>
      v .: "levelnum" <*>
      v .: "entities" <*>
      v .: "inputbuffer" <*>
      v .: "time" <*>
      v .: "settings" <*>
      v .: "rngstuff" <*>
      v .: "animationstate"


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

instance ToJSON Settings where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InputButton where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EntityRecord where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON MenuState where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON StartMenuState where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON PauseMenuState where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON SettingMenuState where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON GhostMovementPattern where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON PlayerEntity where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON PowerState where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON AnimationState where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON RngConstruct where
  toEncoding = genericToEncoding defaultOptions


instance ToJSON RngStuff where
  toJSON (MkRngStuff rngstate rngconstruct) = 
    object ["rngConst" .= rngconstruct]

  toEncoding (MkRngStuff rngstate rngconstruct) = 
    pairs ("rngConst" .= rngconstruct)

instance ToJSON GameState where
  toJSON (MkGameState menustate maze score levelnum entities inputbutton time settings rngstuff animationstate) = 
    object [
      "menuState" .= menustate,
      "maze" .= maze,
      "score" .= score,
      "levelnum" .= levelnum,
      "entities" .= entities,
      "inputbuffer" .= inputbutton,
      "time" .= time,
      "settings" .= settings,
      "animationstate" .= animationstate,
      "rngstuff" .= rngstuff
    ]
  toEncoding (MkGameState menustate maze score levelnum entities inputbutton time settings rngstuff animationstate) =
    pairs (
      "menuState" .= menustate <>
      "maze" .= maze <>
      "score" .= score <>
      "levelnum" .= levelnum <>
      "entities" .= entities <>
      "inputbuffer" .= inputbutton <>
      "time" .= time <>
      "settings" .= settings <>
      "rngstuff" .= rngstuff <>
      "animationstate" .= animationstate
    )

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
  (MkSettings 1 4)
  (hydrateRngStuff $ initialiseRngState $ MkRngConst magicNumber 0)
  NoAnimation

applyLevel :: Level -> GameState -> GameState
applyLevel lvl gs = gs {
  menuState = Playing, 
  maze = levelMaze lvl,
  level = levelNumber lvl, 
  entities = levelEntities lvl,
  inputBuffer = InputNeutral,
  time = 0,
  settings = MkSettings 1 4,
  rngStuff = hydrateRngStuff $ initialiseRngState $ MkRngConst magicNumber 0,
  animationState = NoAnimation}

levelEntities :: Level -> EntityRecord
levelEntities lvl = MkEntityRecord standardPlayer (levelEnemies lvl) (levelFruits lvl) Chase chasePattern

chasePattern :: [Float]
chasePattern = [30,80,30,80,20,4000,4]

standardPlayer :: PlayerEntity
standardPlayer = MkPlayer (15,1) West Weak 1

saveLevel :: IO ()
saveLevel = B.writeFile "levels/level2.json" (encode (gameStateToLevel level2GameState))

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

