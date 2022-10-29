module Controller.MenuController where
import Model
import Model.Menus

inputFromButton :: GameState -> InputButton -> IO GameState
inputFromButton gs@MkGameState {menuState = PauseMenu _} = flip navigatePauseMenu gs
inputFromButton gs@MkGameState {menuState = StartMenu _} = flip navigateStartMenu gs
inputFromButton gs@MkGameState {menuState = SettingMenu} = flip navigateStartMenu gs
inputFromButton gs = const $ return gs


navigatePauseMenu :: InputButton -> GameState -> IO GameState
navigatePauseMenu = undefined


navigateStartMenu :: InputButton -> GameState -> IO GameState
navigateStartMenu = undefined

navigateSettingMenu :: InputButton -> GameState -> IO GameState
navigateSettingMenu = undefined