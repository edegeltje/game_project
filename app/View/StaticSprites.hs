module View.StaticSprites where

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