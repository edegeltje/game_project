module View.ViewGame where

import Graphics.Gloss

import Model.Entities

import View.AnimatedSprites
import View.StaticSprites

import Model

drawConstantSprites :: GameState -> IO Picture
drawConstantSprites MkGameState {
  entities = MkEntityRecord {fruits=_fruits},
  maze = _maze} = do
    let bottomPicture = drawBottomLayer _maze
    return $ pictures [
      bottomPicture
      ]

view :: GameState -> IO Picture
view gs = picturesIO [ return $ drawBottomLayer $ maze gs,
  renderEntitiesIO (entities gs) (time gs)]

renderEntitiesIO :: EntityRecord -> Float -> IO Picture
renderEntitiesIO (MkEntityRecord player enemies fruits) t = picturesIO layerlist
    where
      layerlist = [
        renderRenderableListIO t fruits,
        renderRenderableListIO t enemies,
        renderRenderableIO t player]


renderRenderableIO :: Renderable a => Float -> a -> IO Picture
renderRenderableIO t thing = do
  sprite <- getSpriteIO thing t
  let spritePosition = getPosition thing
  let spriteCoords = toFloatTuple spritePosition
  return $ translate' spriteCoords sprite

renderRenderableListIO :: Renderable a => Float -> [a] -> IO Picture
renderRenderableListIO t = (pictures <$>) . mapM (renderRenderableIO t)
