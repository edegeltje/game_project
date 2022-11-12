module View.ViewMenu where

import Model
import Graphics.Gloss
import Model.Menus
import Model.Entities
import Graphics.Gloss.Interface.IO.Animate
import Model.Settings

view :: GameState -> Picture
view gs@MkGameState{menuState = PauseMenu _}   = pictures [pauseMenuPicture gs]
view gs@MkGameState{menuState = StartMenu _}   = pictures [startMenuPicture gs]
view _ = blank

menuOptionPicture :: Color -> String -> Picture
menuOptionPicture bgColor option = pictures [
        color bgColor (polygon' [
          (0,0),
          (80,0),
          (80,16),
          (0,16)
          ]),
        translate' (0,8) (scale 0.5 0.5 (text option))
        ]

makeSettingMenu :: Settings -> SettingMenuState -> Picture
makeSettingMenu s option = translate' (-32,16) $
  pictures [ translate' (0,-16*i) $ menuOptionPicture (getColor o) name | 
    ((o, name), i) <- zip (getSettingOptions s) [0..]]
      where getColor a = if a == option then green else yellow

makeMenu :: MenuOption a => a -> Picture
makeMenu option = translate' (-32,24) $
  pictures [ translate' (0,-16*i) $ menuOptionPicture (getColor o) name | 
    ((o, name), i) <- zip (optionsWithNames option) [0..]]
      where getColor a = if a == option then green else yellow 

pauseMenuPicture :: GameState -> Picture
pauseMenuPicture MkGameState {menuState = PauseMenu sm@(PauseSettingOption SuperMenu)} = 
  makeMenu sm
pauseMenuPicture gs@MkGameState {menuState = PauseMenu (PauseSettingOption smState)} = 
  makeSettingMenu (settings gs) smState
pauseMenuPicture MkGameState {menuState = PauseMenu pauseOption} = makeMenu pauseOption
pauseMenuPicture _ = blank

startMenuPicture :: GameState -> Picture
startMenuPicture gs@MkGameState {menuState = StartMenu sm@(StartSettingOption SuperMenu)} = 
  showLevel gs $ makeMenu sm
startMenuPicture gs@MkGameState {menuState = StartMenu (StartSettingOption smState)} = 
  makeSettingMenu (settings gs) smState
startMenuPicture gs@MkGameState {menuState = StartMenu startOption} = showLevel gs $ makeMenu startOption
startMenuPicture _ = blank

showLevel :: GameState -> Picture -> Picture
showLevel gs p = pictures [p, color black $ translate' (32, -10) $ scale 0.5 0.5 $ text $ show $ level gs]