module View.StaticSprites where

import qualified Data.Maybe as DMB
import qualified Data.Map as DM
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
import Graphics.Gloss
import Model

import Model.Entities

type SmallDotPicture = Picture
type PowerDotPicture = Picture
type CherryPicture = Picture
type StrawberryPicture = Picture
type OrangePicture = Picture
type ApplePicture = Picture
type MelonPicture = Picture
type GalaxianPicture = Picture
type BellPicture = Picture
type KeyPicture = Picture
type WallPicture = Picture

data ConstantSprites = MkCSprites{
    smallDotSprite :: SmallDotPicture,
    powerDotSprite :: PowerDotPicture,
    appleSprite :: ApplePicture,
    bellSprite :: BellPicture,
    cherrySprite :: CherryPicture,
    galaxianSprite :: GalaxianPicture,
    keySprite :: KeyPicture,
    melonSprite :: MelonPicture,
    orangeSprite :: OrangePicture,
    strawberrySprite :: StrawberryPicture,
    innerCornerWallSprite :: WallPicture,
    straightWallSprite :: WallPicture,
    outerCornerWallSprite :: WallPicture
}


smallDot :: SmallDotPicture
smallDot = color white $ circleSolid 10
powerDot :: PowerDotPicture
powerDot = color white $ circleSolid 20

innerCornerWall :: WallPicture
innerCornerWall = color blue (polygon' [(0.5, 0), (1, 0), (1, 0.5), (0.5, 0.5)])

straightWall :: WallPicture
straightWall = color blue (polygon' [(0.5, 0), (1,0), (1,1),(0.5,1)])

outerCornerWall :: WallPicture
outerCornerWall = color blue (polygon' [(0.5,0), (1,0), (1,1), (0,1), (0,0.5),(0.5,0.5)])

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

constantSpritesIO :: IO ConstantSprites
constantSpritesIO = do
  apple <- appleIO
  bell <- bellIO
  cherry <- cherryIO
  galaxian <- galaxianIO
  key <- keyIO
  melon <- melonIO
  orange <- orangeIO
  strawberry <- strawberryIO
  pure $ 
    MkCSprites 
      smallDot 
      powerDot
      apple
      bell
      cherry
      galaxian
      key
      melon
      orange
      strawberry
      innerCornerWall
      straightWall
      outerCornerWall

dirToAngle :: Direction -> Float
dirToAngle North = 270
dirToAngle East  = 180
dirToAngle South = 90
dirToAngle West  = 0


window = InWindow "Pacman" (800, 600) (10, 10)

testSprite = animate window black (const smallDot)
testSpriteIO = do 
  sprite <- cherryIO
  animate window black $ const sprite

testSprites = do
  cSprites <- constantSpritesIO
  let composedSprites = pictures [
        translate' (0,1) $smallDotSprite    cSprites,
        translate' (1,1) $ cherrySprite     cSprites,
        translate' (2,1) $ strawberrySprite cSprites,
        translate' (3,1) $ orangeSprite     cSprites,
        translate' (4,1) $ appleSprite      cSprites,
        translate' (0,0) $ powerDotSprite   cSprites,
        translate' (1,0) $ melonSprite      cSprites,
        translate' (2,0) $ galaxianSprite   cSprites,
        translate' (3,0) $ bellSprite       cSprites,
        translate' (4,0) $ keySprite        cSprites
        ]
  animate window black $ const composedSprites



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
getPositionContent position maze = DMB.fromMaybe Empty $ DM.lookup position maze


drawWallSprite :: Position -> BottomLayer -> WallPicture
drawWallSprite position maze = rotate (dirToAngle $ orientWall enwsContent) wallSprite 
  where
    x = xposition position
    y = yposition position
    nePosition = MkPosition (x+1) (y+1)
    nwPosition = MkPosition (x-1) (y+1)
    swPosition = MkPosition (x-1) (y-1)
    sePosition = MkPosition (x+1) (y-1)
    enwsContent = map (`getPositionContent` maze) [
      nePosition,
      nwPosition,
      swPosition,
      sePosition
      ] -- this ordering to traverse in counterclockwise direction
    neighbouringWalls = length (filter (== Wall) enwsContent)
    wallSprite = case neighbouringWalls of
      1 -> innerCornerWall
      2 -> straightWall
      3 -> outerCornerWall
      _ -> blank

orientWall :: [BottomLayerContent] -> Direction
orientWall enwsContent = case enwsContent of
  [Wall,_,Wall,Wall] -> West
  [Wall,_,_,Wall] -> South
  [Wall,_,_,_] -> East
  [_,Wall,_,_] -> North
  [_,_,Wall,_] -> West
  _ -> South