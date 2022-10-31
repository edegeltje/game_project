{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Controller.MenuController where
import Model
import Model.Menus
import Model.Settings
import System.Exit

inputFromButton :: GameState -> InputButton -> IO GameState
inputFromButton gs@MkGameState {menuState = PauseMenu _} = navigatePauseMenu gs
inputFromButton gs@MkGameState {menuState = StartMenu _} = navigateStartMenu gs
inputFromButton gs@MkGameState {menuState = SettingMenu _} = navigateSettingMenu gs
inputFromButton gs = const $ return gs


navigatePauseMenu :: GameState -> InputButton -> IO GameState
navigatePauseMenu gs@MkGameState {menuState = PauseMenu ContinueOption} = flip eventPauseMenuContinue gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu PauseSettingOption} = flip eventPauseMenuSetting gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu ExitToStartOption} = flip eventPauseMenuStart gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu ExitToDesktopOption} = flip eventPauseMenuDesktop gs

eventPauseMenuContinue :: InputButton -> GameState -> IO GameState
eventPauseMenuContinue inputUp gs = return (gs {menuState = PauseMenu ExitToDesktopOption})
eventPauseMenuContinue inputDown gs = return (gs {menuState = PauseMenu PauseSettingOption}) 
eventPauseMenuContinue inputSelect gs = return (gs {menuState = Playing})

eventPauseMenuSetting :: InputButton -> GameState -> IO GameState
eventPauseMenuSetting inputUp gs = return (gs {menuState = PauseMenu ContinueOption})
eventPauseMenuSetting inputDown gs = return (gs {menuState = PauseMenu ExitToStartOption})
eventPauseMenuSetting inputSelect gs = return (gs {menuState = SettingMenu VolumeOption})

eventPauseMenuStart :: InputButton -> GameState -> IO GameState
eventPauseMenuStart inputUp gs = return (gs {menuState = PauseMenu PauseSettingOption})
eventPauseMenuStart inputDown gs = return (gs {menuState = PauseMenu ExitToDesktopOption})
eventPauseMenuStart inputSelect gs = return (gs {menuState = StartMenu PlayOption})

eventPauseMenuDesktop :: InputButton -> GameState -> IO GameState
eventPauseMenuDesktop inputUp gs = return (gs {menuState = PauseMenu ExitToStartOption})
eventPauseMenuDesktop inputDown gs = return (gs {menuState = PauseMenu ContinueOption})
eventPauseMenuDesktop inputSelect gs = exitSuccess


navigateStartMenu :: GameState -> InputButton -> IO GameState
navigateStartMenu gs@MkGameState {menuState = StartMenu PlayOption} = flip eventStartMenuPlay gs
navigateStartMenu gs@MkGameState {menuState = StartMenu StartSettingOption} = flip eventStartMenuSetting gs
navigateStartMenu gs@MkGameState {menuState = StartMenu ExitOption} = flip eventStartMenuExit gs

eventStartMenuPlay :: InputButton -> GameState -> IO GameState
eventStartMenuPlay inputUp gs = return (gs {menuState = StartMenu ExitOption})
eventStartMenuPlay inputDown gs = return (gs {menuState = StartMenu StartSettingOption})
eventStartMenuPlay inputSelect gs = return (gs {menuState = Playing})

eventStartMenuSetting :: InputButton -> GameState -> IO GameState
eventStartMenuSetting inputUp gs = return (gs {menuState = StartMenu PlayOption})
eventStartMenuSetting inputDown gs = return (gs {menuState = StartMenu ExitOption})
eventStartMenuSetting inputSelect gs = return (gs {menuState = SettingMenu VolumeOption})

eventStartMenuExit :: InputButton -> GameState -> IO GameState
eventStartMenuExit inputUp gs = return (gs {menuState = StartMenu StartSettingOption})
eventStartMenuExit inputDown gs = return (gs {menuState = StartMenu PlayOption}) 
eventStartMenuExit inputSelect gs = exitSuccess


navigateSettingMenu :: GameState -> InputButton -> IO GameState
navigateSettingMenu gs@MkGameState {menuState = SettingMenu VolumeOption} = flip eventSettingMenuVolume gs
navigateSettingMenu gs@MkGameState {menuState = SettingMenu SpeedOption} = flip eventSettingMenuSpeed gs
--navigateSettingMenu gs@MkGameState {menuState = SettingMenu VolumeOption, settings = currentSettings} = return (gs {})
--    where
--        currentVolume = volume $ settings gs
--        newSettings = currentSettings {volume = newVolume}
--        newVolume = 


eventSettingMenuVolume :: InputButton -> GameState -> IO GameState
eventSettingMenuVolume inputUp gs    = return (gs {menuState = SettingMenu SpeedOption})
eventSettingMenuVolume inputDown gs  = return (gs {menuState = SettingMenu SpeedOption})
eventSettingMenuVolume inputLeft gs@MkGameState {settings= currentSetting@MkSettings{volume = 0}} 
                                     = return (gs {settings = currentSetting{volume = 0}})
eventSettingMenuVolume inputLeft gs@MkGameState {settings= currentSetting}   
                                     = return (gs {settings = currentSetting{volume = currentVolume - 1}})
                                     where currentVolume = volume $ settings gs
eventSettingMenuVolume inputRight gs@MkGameState {settings= currentSetting@MkSettings{volume = 10}}  
                                     = return (gs {settings = currentSetting{volume = 10}})
eventSettingMenuVolume inputRight gs@MkGameState {settings= currentSetting}  
                                     = return (gs {settings = currentSetting{volume = currentVolume + 1}})
                                    where currentVolume = volume $ settings gs

eventSettingMenuSpeed :: InputButton -> GameState -> IO GameState
eventSettingMenuSpeed inputUp gs = return (gs {menuState = SettingMenu VolumeOption})
eventSettingMenuSpeed inputDown gs = return (gs {menuState = SettingMenu VolumeOption})
eventSettingMenuSpeed inputLeft gs@MkGameState {settings= currentSetting@MkSettings{gameSpeed = 10}}  
                                     = return (gs {settings = currentSetting{gameSpeed = 10}})
eventSettingMenuSpeed inputLeft gs@MkGameState {settings= currentSetting} 
                                     = return (gs {settings = currentSetting{gameSpeed = currentSpeed - 1}})
                                     where currentSpeed = gameSpeed $ settings gs
eventSettingMenuSpeed inputRight gs@MkGameState {settings= currentSetting@MkSettings{gameSpeed = 60}}  
                                     = return (gs {settings = currentSetting{gameSpeed = 60}})
eventSettingMenuSpeed inputRight gs@MkGameState {settings= currentSetting}  
                                    = return (gs {settings = currentSetting{gameSpeed = currentSpeed + 1}})
                                    where currentSpeed = gameSpeed $ settings gs
