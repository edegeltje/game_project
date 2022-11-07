module View.ViewGame where

import Graphics.Gloss

import Model.Entities

import View.AnimatedSprites
import View.StaticSprites

import Model
import Model.Settings (tILESIZE)

drawConstantSprites :: GameState -> IO Picture
drawConstantSprites MkGameState {
  entities = MkEntityRecord {fruits=_fruits},
  maze = _maze} = do
    let bottomPicture = drawBottomLayer _maze
    return $ pictures [
      bottomPicture
      ]

view :: FruitSprites -> GameState -> Picture
view fs gs = pictures [drawBottomLayer $ maze gs,
  renderEntities fs (entities gs) (time gs),
  translate' (tILESIZE, tILESIZE*4) (scale 0.2 0.2 (color white (text (show (score gs)))))
  ]

renderEntities :: FruitSprites -> EntityRecord -> Float -> Picture
renderEntities fs (MkEntityRecord player enemies fruits) t = pictures layerlist
    where
      layerlist = [
        renderRenderableList fs t fruits,
        renderRenderableList fs t enemies,
        renderRenderable fs t player]


renderRenderable :: Renderable a => FruitSprites -> Float -> a -> Picture
renderRenderable fs t thing = translate' (toFloatTuple spritePosition) sprite where
  sprite = getSprite fs thing t
  spritePosition = getPosition thing
  spriteCoords = toFloatTuple spritePosition

renderRenderableList :: Renderable a => FruitSprites -> Float -> [a] -> Picture
renderRenderableList fs t = pictures . map (renderRenderable fs t)

