module View.StaticSprites where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
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
innerCornerWall = color blue (polygon [(40, 0), (80, 0), (80, 40), (40, 40)])

straightWall :: WallPicture
straightWall = color blue (polygon [(40, 0), (80,0), (80,80),(40,80)])

outerCornerWall :: WallPicture
outerCornerWall = color blue (polygon [(40,0), (80,0), (80,80), (0,80), (0,40),(40,40)])

appleBitmapPath :: FilePath
appleBitmapPath = "bitmaps/apple.bmp"

bellBitmapPath :: FilePath
bellBitmapPath = "bitmaps/bell.bmp"

cherryBitmapPath :: FilePath
cherryBitmapPath = "bitmaps/cherry.bmp"

galaxianBitmapPath :: FilePath
galaxianBitmapPath = "bitmaps/galaxian.bmp"

keyBitmapPath :: FilePath
keyBitmapPath = "bitmaps/key.bmp"

melonBitmapPath :: FilePath
melonBitmapPath = "bitmaps/melon.bmp"

orangeBitmapPath :: FilePath
orangeBitmapPath = "bitmaps/orange.bmp"

strawberryBitmapPath :: FilePath
strawberryBitmapPath = "bitmaps/strawberry.bmp"

appleIO :: IO ApplePicture
appleIO = loadBMP appleBitmapPath

bellIO :: IO BellPicture
bellIO = loadBMP bellBitmapPath

cherryIO :: IO CherryPicture
cherryIO = loadBMP cherryBitmapPath 

galaxianIO :: IO GalaxianPicture
galaxianIO = loadBMP galaxianBitmapPath

keyIO :: IO KeyPicture
keyIO = loadBMP keyBitmapPath

melonIO :: IO MelonPicture
melonIO = loadBMP melonBitmapPath

orangeIO :: IO OrangePicture
orangeIO = loadBMP orangeBitmapPath

strawberryIO :: IO StrawberryPicture
strawberryIO = loadBMP strawberryBitmapPath

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


    

window = InWindow "Pacman" (800, 600) (10, 10)

testSprite = animate window black (const smallDot)
testSpriteIO = do 
  sprite <- cherryIO
  animate window black $ const sprite

testSprites = do
  cSprites <- constantSpritesIO
  let composedSprites = pictures [
        translate 0 80   $smallDotSprite    cSprites,
        translate 80 80  $ cherrySprite     cSprites,
        translate 160 80 $ strawberrySprite cSprites,
        translate 240 80 $ orangeSprite     cSprites,
        translate 320 80 $ appleSprite      cSprites,
        translate 0 0    $ powerDotSprite   cSprites,
        translate 80 0   $ melonSprite      cSprites,
        translate 160 0  $ galaxianSprite   cSprites,
        translate 240 0  $ bellSprite       cSprites,
        translate 320 0  $ keySprite        cSprites
        ]
  animate window black $ const composedSprites
