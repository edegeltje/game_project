module View.ViewMenu where

import Model
import Graphics.Gloss
import Model.Menus
import Model.Entities
import Graphics.Gloss.Interface.IO.Animate
import Model.Settings

view :: GameState -> IO Picture
view gs@MkGameState{menuState = PauseMenu _}   = picturesIO [return (pauseMenuPicture gs)]
view gs@MkGameState{menuState = StartMenu _}   = picturesIO [return (startMenuPicture gs)]
view gs@MkGameState{menuState = SettingMenu _} = picturesIO [return (settingMenuPicture gs)]


pauseMenuPicture :: GameState -> Picture
pauseMenuPicture gs@MkGameState {menuState = PauseMenu ContinueOption} = translate' (-tILESIZE * 4, tILESIZE * 2) (pictures [pictures [color green (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]),  translate' (0, tILESIZE) (scale 0.5 0.5 (text "Continue"))],
                                                                                                        translate' (0, -(tILESIZE * 2)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Settings"))]),
                                                                                                        translate' (0, -(tILESIZE * 4)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to start"))]),
                                                                                                        translate' (0, -(tILESIZE * 6)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to desktop"))])
                                                                                                         ])
pauseMenuPicture gs@MkGameState {menuState = PauseMenu PauseSettingOption} = translate' (-tILESIZE * 4, tILESIZE * 2) (pictures [pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]),  translate' (0, tILESIZE) (scale 0.5 0.5 (text "Continue"))],
                                                                                                        translate' (0, -(tILESIZE * 2)) (pictures [color green (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Settings"))]),
                                                                                                        translate' (0, -(tILESIZE * 4)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to start"))]),
                                                                                                        translate' (0, -(tILESIZE * 6)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to desktop"))])
                                                                                                         ])
pauseMenuPicture gs@MkGameState {menuState = PauseMenu ExitToStartOption} = translate' (-tILESIZE * 4, tILESIZE * 2) (pictures [pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]),  translate' (0, tILESIZE) (scale 0.5 0.5 (text "Continue"))],
                                                                                                        translate' (0, -(tILESIZE * 2)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Settings"))]),
                                                                                                        translate' (0, -(tILESIZE * 4)) (pictures [color green (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to start"))]),
                                                                                                        translate' (0, -(tILESIZE * 6)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to desktop"))])
                                                                                                         ])
pauseMenuPicture gs                                                       = translate' (-tILESIZE * 4, tILESIZE * 2) (pictures [pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]),  translate' (0, tILESIZE) (scale 0.5 0.5 (text "Continue"))],
                                                                                                        translate' (0, -(tILESIZE * 2)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Settings"))]),
                                                                                                        translate' (0, -(tILESIZE * 4)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to start"))]),
                                                                                                        translate' (0, -(tILESIZE * 6)) (pictures [color green (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to desktop"))])
                                                                                                         ])

startMenuPicture :: GameState -> Picture
startMenuPicture gs@MkGameState {menuState = StartMenu PlayOption}         = translate' (-tILESIZE * 4, tILESIZE * 2) (pictures [pictures [color green (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]),  translate' (0, tILESIZE) (scale 0.5 0.5 (text "Play"))],
                                                                                                        translate' (0, -(tILESIZE * 2)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Settings"))]),
                                                                                                        translate' (0, -(tILESIZE * 4)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to desktop"))])
                                                                                                         ])
startMenuPicture gs@MkGameState {menuState = StartMenu StartSettingOption} = translate' (-tILESIZE * 4, tILESIZE * 2) (pictures [pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]),  translate' (0, tILESIZE) (scale 0.5 0.5 (text "Play"))],
                                                                                                        translate' (0, -(tILESIZE * 2)) (pictures [color green (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Settings"))]),
                                                                                                        translate' (0, -(tILESIZE * 4)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to desktop"))])
                                                                                                         ])
startMenuPicture gs                                                        = translate' (-tILESIZE * 4, tILESIZE * 2) (pictures [pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]),  translate' (0, tILESIZE) (scale 0.5 0.5 (text "Play"))],
                                                                                                        translate' (0, -(tILESIZE * 2)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Settings"))]),
                                                                                                        translate' (0, -(tILESIZE * 4)) (pictures [color green (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Exit to desktop"))])
                                                                                                         ])
                                                                                                    
settingMenuPicture :: GameState -> Picture
settingMenuPicture gs@MkGameState {menuState = SettingMenu VolumeOption} = translate' (-tILESIZE * 4, tILESIZE * 2) (pictures [pictures [color green (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]),  translate' (0, tILESIZE) (scale 0.5 0.5 (text "Volume")), translate' (tILESIZE * 8, tILESIZE) (scale 0.5 0.5 (text currentVolume))],
                                                                                                        translate' (0, -(tILESIZE * 2)) (pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Speed")), translate' (tILESIZE * 8, tILESIZE) (scale 0.5 0.5 (text currentSpeed))])
                                                                                                         ])
                                                                            where
                                                                                currentVolume = show (volume $ settings gs)
                                                                                currentSpeed = show (gameSpeed $ settings gs)
settingMenuPicture gs                                                    = translate' (-tILESIZE * 4, tILESIZE * 2) (pictures [pictures [color yellow (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]),  translate' (0, tILESIZE) (scale 0.5 0.5 (text "Volume")), translate' (tILESIZE * 8, tILESIZE) (scale 0.5 0.5 (text currentVolume))],
                                                                                                        translate' (0, -(tILESIZE * 2)) (pictures [color green (polygon' [(0,0), (tILESIZE * 10,0), (tILESIZE * 10,tILESIZE * 2), (0,tILESIZE * 2)]), translate' (0, tILESIZE) (scale 0.5 0.5 (text "Speed")), translate' (tILESIZE * 8, tILESIZE) (scale 0.5 0.5 (text currentSpeed))])
                                                                                                         ])
                                                                            where
                                                                                currentVolume = show (volume $ settings gs)
                                                                                currentSpeed = show (gameSpeed $ settings gs)
