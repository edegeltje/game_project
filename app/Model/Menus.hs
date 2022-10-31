module Model.Menus where

data MenuState = PauseMenu PauseMenuState | StartMenu StartMenuState | SettingMenu SettingMenuState | Playing

data PauseMenuState = ContinueOption | PauseSettingOption | ExitToStartOption | ExitToDesktopOption

data StartMenuState = PlayOption | StartSettingOption | ExitOption

data SettingMenuState = VolumeOption | SpeedOption

