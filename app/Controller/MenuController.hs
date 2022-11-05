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
navigatePauseMenu gs@MkGameState {menuState = PauseMenu ContinueOption} =
  flip eventPauseMenuContinue gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu PauseSettingOption} =
  flip eventPauseMenuSetting gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu ExitToStartOption} =
  flip eventPauseMenuStart gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu ExitToDesktopOption} =
  flip eventPauseMenuDesktop gs
navigatePauseMenu gs = const $ return gs

eventPauseMenuContinue :: InputButton -> GameState -> IO GameState
eventPauseMenuContinue InputUp gs = return (gs {menuState = PauseMenu ExitToDesktopOption})
eventPauseMenuContinue InputDown gs = return (gs {menuState = PauseMenu PauseSettingOption}) 
eventPauseMenuContinue InputSelect gs = return (gs {menuState = Playing})

eventPauseMenuSetting :: InputButton -> GameState -> IO GameState
eventPauseMenuSetting InputUp gs = return (gs {menuState = PauseMenu ContinueOption})
eventPauseMenuSetting InputDown gs = return (gs {menuState = PauseMenu ExitToStartOption})
eventPauseMenuSetting InputSelect gs = return (gs {menuState = SettingMenu VolumeOption})

eventPauseMenuStart :: InputButton -> GameState -> IO GameState
eventPauseMenuStart InputUp gs = return (gs {menuState = PauseMenu PauseSettingOption})
eventPauseMenuStart InputDown gs = return (gs {menuState = PauseMenu ExitToDesktopOption})
eventPauseMenuStart InputSelect gs = return (gs {menuState = StartMenu PlayOption})

eventPauseMenuDesktop :: InputButton -> GameState -> IO GameState
eventPauseMenuDesktop InputUp gs = return (gs {menuState = PauseMenu ExitToStartOption})
eventPauseMenuDesktop InputDown gs = return (gs {menuState = PauseMenu ContinueOption})
eventPauseMenuDesktop InputSelect gs = exitSuccess


navigateStartMenu :: GameState -> InputButton -> IO GameState
navigateStartMenu gs@MkGameState {menuState = StartMenu PlayOption} =
  flip eventStartMenuPlay gs
navigateStartMenu gs@MkGameState {menuState = StartMenu StartSettingOption} = 
  flip eventStartMenuSetting gs
navigateStartMenu gs@MkGameState {menuState = StartMenu ExitOption} = 
  flip eventStartMenuExit gs
navigateStartMenu gs = const $ return gs

eventStartMenuPlay :: InputButton -> GameState -> IO GameState
eventStartMenuPlay InputUp gs = return (gs {menuState = StartMenu ExitOption})
eventStartMenuPlay InputDown gs = return (gs {menuState = StartMenu StartSettingOption})
eventStartMenuPlay InputSelect gs = return (gs {menuState = Playing})

eventStartMenuSetting :: InputButton -> GameState -> IO GameState
eventStartMenuSetting InputUp gs = return (gs {menuState = StartMenu PlayOption})
eventStartMenuSetting InputDown gs = return (gs {menuState = StartMenu ExitOption})
eventStartMenuSetting InputSelect gs = return (gs {menuState = SettingMenu VolumeOption})

eventStartMenuExit :: InputButton -> GameState -> IO GameState
eventStartMenuExit InputUp gs = return (gs {menuState = StartMenu StartSettingOption})
eventStartMenuExit InputDown gs = return (gs {menuState = StartMenu PlayOption}) 
eventStartMenuExit InputSelect gs = exitSuccess


navigateSettingMenu :: GameState -> InputButton -> IO GameState
navigateSettingMenu gs@MkGameState {menuState = SettingMenu VolumeOption} =
  flip eventSettingMenuVolume gs
navigateSettingMenu gs@MkGameState {menuState = SettingMenu SpeedOption} = 
  flip eventSettingMenuSpeed gs
navigateSettingMenu gs = const $ return gs

eventSettingMenuVolume :: InputButton -> GameState -> IO GameState
eventSettingMenuVolume InputUp gs    = return (gs {menuState = SettingMenu SpeedOption})
eventSettingMenuVolume InputDown gs  = return (gs {menuState = SettingMenu SpeedOption})
eventSettingMenuVolume InputLeft gs@MkGameState {settings= currentSetting@MkSettings{volume = 0}} 
                                     = return (gs {settings = currentSetting{volume = 0}})
eventSettingMenuVolume InputLeft gs@MkGameState {settings= currentSetting}   
                                     = return (gs {settings = currentSetting{volume = currentVolume - 1}})
                                     where currentVolume = volume $ settings gs
eventSettingMenuVolume InputRight gs@MkGameState {settings= currentSetting@MkSettings{volume = 10}}  
                                     = return (gs {settings = currentSetting{volume = 10}})
eventSettingMenuVolume InputRight gs@MkGameState {settings= currentSetting}  
                                     = return (gs {settings = currentSetting{volume = currentVolume + 1}})
                                    where currentVolume = volume $ settings gs

eventSettingMenuSpeed :: InputButton -> GameState -> IO GameState
eventSettingMenuSpeed InputUp gs = return (gs {menuState = SettingMenu VolumeOption})
eventSettingMenuSpeed InputDown gs = return (gs {menuState = SettingMenu VolumeOption})
eventSettingMenuSpeed InputLeft gs@MkGameState {settings= currentSetting@MkSettings{gameSpeed = 10}}  
                                     = return (gs {settings = currentSetting{gameSpeed = 10}})
eventSettingMenuSpeed InputLeft gs@MkGameState {settings= currentSetting} 
                                     = return (gs {settings = currentSetting{gameSpeed = currentSpeed - 1}})
                                     where currentSpeed = gameSpeed $ settings gs
eventSettingMenuSpeed InputRight gs@MkGameState {settings= currentSetting@MkSettings{gameSpeed = 60}}  
                                     = return (gs {settings = currentSetting{gameSpeed = 60}})
eventSettingMenuSpeed InputRight gs@MkGameState {settings= currentSetting}  
                                    = return (gs {settings = currentSetting{gameSpeed = currentSpeed + 1}})
                                    where currentSpeed = gameSpeed $ settings gs
