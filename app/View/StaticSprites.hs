{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module View.StaticSprites where

import qualified Data.Maybe as DMB
import qualified Data.Map as DM
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
import Graphics.Gloss
import Model

import Model.Entities


fruitSpritesIO = do
  apple <- appleIO
  bell <- bellIO
  cherry <- cherryIO
  galaxian <- galaxianIO
  key <- keyIO
  melon <- melonIO
  orange <- orangeIO
  strawberry <- strawberryIO
  return $ MkFSprites
    apple
    bell
    cherry
    galaxian
    key
    melon
    orange
    strawberry
  


smallDot :: SmallDotPicture
smallDot = color white $ circleSolid' (1/8)
powerDot :: PowerDotPicture
powerDot = color white $ circleSolid' (1/4)

innerCornerWall :: WallPicture
innerCornerWall = color blue $ polygon' 
  [(0, 0), (0.5, 0), (0.5, -0.5), (0, -0.5)]

straightWall :: WallPicture
straightWall = color blue $ polygon'
  [(0, -0.5), (0.5,-0.5), (0.5,0.5),(0,0.5)]

outerCornerWall :: WallPicture
outerCornerWall = color blue $ polygon'
  [(0,0), (-0.5,0), (-0.5,0.5), (0.5,0.5), (0.5,-0.5),(0,-0.5)]

appleIO :: IO ApplePicture
appleIO = loadBMP "bitmaps/apple.bmp"

bellIO :: IO BellPicture
bellIO = loadBMP "bitmaps/bell.bmp"

cherryIO :: IO CherryPicture
cherryIO = loadBMP "bitmaps/cherry.bmp" 

galaxianIO :: IO GalaxianPicture
galaxianIO = loadBMP "bitmaps/galaxian.bmp"

keyIO :: IO KeyPicture
keyIO = loadBMP "bitmaps/key.bmp"

melonIO :: IO MelonPicture
melonIO = loadBMP "bitmaps/melon.bmp"

orangeIO :: IO OrangePicture
orangeIO = loadBMP "bitmaps/orange.bmp"

strawberryIO :: IO StrawberryPicture
strawberryIO = loadBMP "bitmaps/strawberry.bmp"

instance Renderable Fruit where
  getSprite fs MkFruit {fruitType = ft} _ = uncurry scale (tileToPoint (1/8,1/8)) $
    case ft of
      Cherry -> cherrySprite fs
      Strawberry -> strawberrySprite fs
      Orange -> orangeSprite fs
      Apple -> appleSprite fs
      Melon -> melonSprite fs
      Galaxian -> galaxianSprite fs
      Bell -> bellSprite fs
      Key -> keySprite fs


dirToAngle :: Direction -> Float
dirToAngle North = 180
dirToAngle East  = 270
dirToAngle South = 0
dirToAngle West  = 90


window = InWindow "Pacman" (800, 600) (10, 10)

drawBottomLayer :: BottomLayer -> Picture
drawBottomLayer maze = DM.foldrWithKey' addToPicture blank maze
  where
    addToPicture position _ canvas = pictures [canvas, placedSpriteFromContext position maze]
-- (Position -> BottomLayerContent -> Picture -> Picture)


placedSpriteFromContext :: Position -> BottomLayer -> Picture
placedSpriteFromContext position maze = translate' (toFloatTuple position) $ 
  spriteFromContext position maze

spriteFromContext :: Position -> BottomLayer -> Picture
spriteFromContext position maze = case getPositionContent position maze of
  Empty    -> blank
  SmallDot -> smallDot
  PowerDot -> powerDot
  Wall     -> drawWallSprite position maze

getPositionContent :: Position -> BottomLayer -> BottomLayerContent
getPositionContent position maze = DMB.fromMaybe Wall $ DM.lookup position maze


drawWallSprite :: Position -> BottomLayer -> WallPicture
drawWallSprite pos maze = rotate (dirToAngle dir) wallSprite 
  where
    (wallSprite,dir) = getWallSpriteAndOrientation $ getNeighbours pos maze

getNeighbours :: Position -> BottomLayer -> [BottomLayerContent]
getNeighbours (x,y) maze = map (`getPositionContent` maze) [
  nwPosition, nPosition, nePosition,
  wPosition, {-position-} ePosition,
  swPosition, sPosition, sePosition] 
    where
    ePosition  = (x+1,y  )
    nePosition = (x+1,y+1)
    nPosition  = (x  ,y+1)
    nwPosition = (x-1,y+1)
    wPosition  = (x-1,y  )
    swPosition = (x-1,y-1)
    sPosition  = (x  ,y-1)
    sePosition = (x+1,y-1)

getWallSpriteAndOrientation :: [BottomLayerContent] -> (WallPicture, Direction)
getWallSpriteAndOrientation enwsContent = case enwsContent of
      [ Wall,Wall,Wall,
        Wall,     Wall,
        Wall,Wall,Wall] -> (blank, North)
      
      [ _   ,Wall,Wall,
        Wall,     Wall,
        Wall,Wall,Wall] -> (outerCornerWall, West)
      
      [ Wall,Wall,_   ,
        Wall,     Wall,
        Wall,Wall,Wall] -> (outerCornerWall, North)
      
      [ Wall,Wall,Wall,
        Wall,     Wall,
        Wall,Wall,_   ] -> (outerCornerWall, East)
      
      [ Wall,Wall,Wall,
        Wall,     Wall,
        _   ,Wall,Wall] -> (outerCornerWall, South)
      
      [ _   ,_   ,_   ,
        Wall,     Wall,
        _   ,Wall,_   ] -> (straightWall, West)

      [ _   ,Wall,_   ,
        Wall,     _   ,
        _   ,Wall,_   ] -> (straightWall, North)
      
      [ _   ,Wall,_   ,
        Wall,     Wall,
        _   ,_   ,_   ] -> (straightWall, East)
      
      [ _   ,Wall,_   ,
        _   ,     Wall,
        _   ,Wall,_   ] -> (straightWall, South)
      
      [ _   ,Wall,_   ,
        Wall,     _   ,
        _   ,_   ,_   ] -> (innerCornerWall, North)
      
      [ _   ,Wall,_   ,
        _   ,     Wall,
        _   ,_   ,_   ] -> (innerCornerWall, East)
      
      [ _   ,_   ,_   ,
        _   ,     Wall,
        _   ,Wall,_   ] -> (innerCornerWall, South)
      
      [ _   ,_   ,_   ,
        Wall,     _   ,
        _   ,Wall,_   ] -> (innerCornerWall, West)
      
      _                 -> (blank, North)
