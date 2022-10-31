module View.ViewGame where 

import Graphics.Gloss

import Model.Entities

import View.AnimatedSprites
import View.StaticSprites

import Model

data Sprites = MkSprites {
  constantSprites :: ConstantSprites,
  animatedSprites :: AnimatedSprites
}

view :: GameState -> IO Picture
view = undefined

renderEntities :: EntityRecord -> IO Picture
renderEntities (MkEntityRecord player enemies fruits) = undefined

renderPlayer :: AnimatedSprites -> Picture
renderPlayer = undefined