module Model.Menus where

data MenuState = PauseMenu PauseMenuState | StartMenu StartMenuState | SettingMenu | Playing

data PauseMenuState = ContinueOption | Settingoption | ExitToStartOption | ExitToDesktopOption

data StartMenuState = PlayOption | SettingOption | ExitOption
