module View.ViewGame where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
import Model.Entities
import Model.Menus

import View.AnimatedSprites
import View.StaticSprites

import Model
import Model.Settings (tILESIZE)
import Data.Fixed
import Controller.GameController

drawConstantSprites :: GameState -> IO Picture
drawConstantSprites MkGameState {
  entities = MkEntityRecord {fruits=_fruits},
  maze = _maze} = do
    let bottomPicture = drawBottomLayer _maze
    return $ pictures [
      bottomPicture
      ]

view :: FruitSprites -> GameState -> Picture
view _ gs@MkGameState {animationState = GameOver t} =  
  color white $ translate' (-30,0) $ scale 0.5 0.5 $ pictures [
   text "Game Over"
  ]
view _ gs@MkGameState {animationState = WinScreen t} = 
  color white $ translate' (-30,0) $ scale 0.5 0.5 $ pictures [
   text "Congratulations, You Win!"
  ]
view fs gs = pictures [drawBottomLayer $ maze gs,
  renderEntities fs (entities gs) (time gs),
  translate' (8, 32) $ translate' (-20,0) $ scale 0.2 0.2 $ 
  color white $ text $ show $ score gs
  ]

renderEntities :: FruitSprites -> EntityRecord -> Float -> Picture
renderEntities fs (MkEntityRecord player enemies fruits _ _) t = pictures layerlist
    where
      layerlist = [
        renderRenderableList fs t fruits,
        renderRenderableList fs t enemies,
        renderRenderable fs t player]


renderRenderable :: Renderable a => FruitSprites -> Float -> a -> Picture
renderRenderable fs t thing = translate' (toFloatTuple spritePosition A.+ spriteOffset) sprite where
  sprite = getSprite fs thing t
  spritePosition = position thing
  spriteCoords = toFloatTuple spritePosition
  spriteOffset = speed thing A.* (mod' t 1 `floatTimes` dirToPos (direction thing))

renderRenderableList :: Renderable a => FruitSprites -> Float -> [a] -> Picture
renderRenderableList fs t = pictures . map (renderRenderable fs t)
