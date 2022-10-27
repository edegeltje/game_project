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
    cherrySprite :: CherryPicture,
    strawberrySprite :: StrawberryPicture,
    orangeSprite :: OrangePicture,
    appleSprite :: ApplePicture,
    melonSprite :: MelonPicture,
    galaxianSprite :: GalaxianPicture,
    bellSprite :: BellPicture,
    keySprite :: KeyPicture,
    wallSprite :: WallPicture
}


smallDot = translate 40 40 $ Circle 10
powerDot = translate 40 40 $ Circle 20

cherryBitmapPath :: FilePath
cherryBitmapPath = "bitmaps/cherry.bmp"
cherry = loadBMP cherryBitmapPath


window = InWindow "Pacman" (800, 600) (10, 10) 

testSprite = animate window black (const smallDot)
testSpriteIO = do 
  sprite <- cherry
  animate window black (const sprite)