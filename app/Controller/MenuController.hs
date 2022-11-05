{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Controller.MenuController where
import Model
import Model.Menus
import Model.Settings
import System.Exit

inputFromButton :: GameState -> InputButton -> IO GameState
inputFromButton gs@MkGameState {menuState = PauseMenu _} = navigatePauseMenu gs
inputFromButton gs@MkGameState {menuState = StartMenu _} = navigateStartMenu gs
inputFromButton gs = const $ return gs


navigatePauseMenu :: GameState -> InputButton -> IO GameState
navigatePauseMenu gs@MkGameState {menuState = PauseMenu ContinueOption} =
  flip eventPauseMenuContinue gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu (PauseSettingOption SuperMenu)} =
  flip eventPauseMenuSetting gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu ExitToStartOption} =
  flip eventPauseMenuStart gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu ExitToDesktopOption} =
  flip eventPauseMenuDesktop gs
navigatePauseMenu gs@MkGameState {menuState = PauseMenu (PauseSettingOption _)} =
  navigateSettingMenu gs

navigatePauseMenu gs = const $ return gs

eventPauseMenuContinue :: InputButton -> GameState -> IO GameState
eventPauseMenuContinue InputUp gs = return (gs {menuState = PauseMenu ExitToDesktopOption})
eventPauseMenuContinue InputDown gs = return (gs {menuState = PauseMenu (PauseSettingOption SuperMenu)}) 
eventPauseMenuContinue InputSelect gs = return (gs {menuState = Playing})
eventPauseMenuContinue _ gs = return gs


eventPauseMenuSetting :: InputButton -> GameState -> IO GameState
eventPauseMenuSetting InputUp gs = return (gs {menuState = PauseMenu ContinueOption})
eventPauseMenuSetting InputDown gs = return (gs {menuState = PauseMenu ExitToStartOption})
eventPauseMenuSetting InputSelect gs = return (gs {menuState = PauseMenu (PauseSettingOption VolumeOption)})
eventPauseMenuSetting _ gs = return gs


eventPauseMenuStart :: InputButton -> GameState -> IO GameState
eventPauseMenuStart InputUp gs = return (gs {menuState = PauseMenu (PauseSettingOption SuperMenu)})
eventPauseMenuStart InputDown gs = return (gs {menuState = PauseMenu ExitToDesktopOption})
eventPauseMenuStart InputSelect gs = return (gs {menuState = StartMenu PlayOption})
eventPauseMenuStart _ gs = return gs

eventPauseMenuDesktop :: InputButton -> GameState -> IO GameState
eventPauseMenuDesktop InputUp gs = return (gs {menuState = PauseMenu ExitToStartOption})
eventPauseMenuDesktop InputDown gs = return (gs {menuState = PauseMenu ContinueOption})
eventPauseMenuDesktop InputSelect gs = exitSuccess
eventPauseMenuDesktop _ gs = return gs


navigateStartMenu :: GameState -> InputButton -> IO GameState
navigateStartMenu gs@MkGameState {menuState = StartMenu PlayOption} =
  flip eventStartMenuPlay gs
navigateStartMenu gs@MkGameState {menuState = StartMenu (StartSettingOption SuperMenu)} = 
  flip eventStartMenuSetting gs
navigateStartMenu gs@MkGameState {menuState = StartMenu ExitOption} = 
  flip eventStartMenuExit gs
navigateStartMenu gs@MkGameState {menuState = StartMenu (StartSettingOption _)} =
  navigateSettingMenu gs
navigateStartMenu gs = const $ return gs

eventStartMenuPlay :: InputButton -> GameState -> IO GameState
eventStartMenuPlay InputUp gs = return (gs {menuState = StartMenu ExitOption})
eventStartMenuPlay InputDown gs = return (gs {menuState = StartMenu (StartSettingOption SuperMenu)})
eventStartMenuPlay InputSelect gs = return (gs {menuState = Playing})
eventStartMenuPlay _ gs = return gs

eventStartMenuSetting :: InputButton -> GameState -> IO GameState
eventStartMenuSetting InputUp gs = return (gs {menuState = StartMenu PlayOption})
eventStartMenuSetting InputDown gs = return (gs {menuState = StartMenu ExitOption})
eventStartMenuSetting InputSelect gs = return (gs {menuState = StartMenu (StartSettingOption VolumeOption)})
eventStartMenuSetting _ gs = return gs

eventStartMenuExit :: InputButton -> GameState -> IO GameState
eventStartMenuExit InputUp gs = return (gs {menuState = StartMenu (StartSettingOption SuperMenu)})
eventStartMenuExit InputDown gs = return (gs {menuState = StartMenu PlayOption}) 
eventStartMenuExit InputSelect gs = exitSuccess
eventStartMenuExit _ gs = return gs

navigateSettingMenu :: GameState -> InputButton -> IO GameState
navigateSettingMenu gs@MkGameState {menuState = StartMenu (StartSettingOption VolumeOption)} =
  flip eventSettingMenuVolume gs
navigateSettingMenu gs@MkGameState {menuState = PauseMenu (PauseSettingOption VolumeOption)} =
  flip eventSettingMenuVolume gs
navigateSettingMenu gs@MkGameState {menuState = StartMenu (StartSettingOption SpeedOption)} = 
  flip eventSettingMenuSpeed gs
navigateSettingMenu gs@MkGameState {menuState = PauseMenu (PauseSettingOption SpeedOption)} =
  flip eventSettingMenuSpeed gs
navigateSettingMenu gs = const $ return gs

eventSettingMenuVolume :: InputButton -> GameState -> IO GameState
eventSettingMenuVolume InputUp gs@MkGameState {menuState = StartMenu _} = 
  return (gs {menuState = StartMenu (StartSettingOption SpeedOption)})
eventSettingMenuVolume InputDown gs@MkGameState {menuState = StartMenu _}  = 
  return (gs {menuState = StartMenu (StartSettingOption SpeedOption)})
eventSettingMenuVolume InputUp gs@MkGameState {menuState = PauseMenu _}    = 
  return (gs {menuState = PauseMenu (PauseSettingOption SpeedOption)})
eventSettingMenuVolume InputDown gs@MkGameState {menuState = PauseMenu _}  = 
  return (gs {menuState = PauseMenu (PauseSettingOption SpeedOption)})
eventSettingMenuVolume InputBack gs@MkGameState {menuState = StartMenu _} = 
  return $ gs {menuState = StartMenu (StartSettingOption SuperMenu)}
eventSettingMenuVolume InputBack gs@MkGameState {menuState = PauseMenu _} = 
  return $ gs {menuState = PauseMenu (PauseSettingOption SuperMenu)}

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
eventSettingMenuVolume _ gs = return gs

eventSettingMenuSpeed :: InputButton -> GameState -> IO GameState
eventSettingMenuSpeed InputUp gs@MkGameState {menuState = StartMenu _} = 
  return $ gs {menuState = StartMenu (StartSettingOption VolumeOption)}
eventSettingMenuSpeed InputDown gs@MkGameState {menuState = StartMenu _} = 
  return $ gs {menuState = StartMenu (StartSettingOption VolumeOption)}
eventSettingMenuSpeed InputUp gs@MkGameState {menuState = PauseMenu _} = 
  return $ gs {menuState = PauseMenu (PauseSettingOption VolumeOption)}
eventSettingMenuSpeed InputDown gs@MkGameState {menuState = PauseMenu _} = 
  return $ gs {menuState = PauseMenu (PauseSettingOption VolumeOption)}

eventSettingMenuSpeed InputBack gs@MkGameState {menuState = StartMenu _} = 
  return $ gs {menuState = StartMenu (StartSettingOption SuperMenu)}
eventSettingMenuSpeed InputBack gs@MkGameState {menuState = PauseMenu _} = 
  return $ gs {menuState = PauseMenu (PauseSettingOption SuperMenu)}


eventSettingMenuSpeed InputLeft gs@MkGameState {settings= currentSetting@MkSettings{gameSpeed = 10}}  
                                     = return gs
eventSettingMenuSpeed InputLeft gs@MkGameState {settings= currentSetting} 
                                     = return (gs {settings = currentSetting{gameSpeed = currentSpeed - 1}})
                                     where currentSpeed = gameSpeed $ settings gs
eventSettingMenuSpeed InputRight gs@MkGameState {settings= currentSetting@MkSettings{gameSpeed = 60}}  
                                     = return gs
eventSettingMenuSpeed InputRight gs@MkGameState {settings= currentSetting}  
                                    = return (gs {settings = currentSetting{gameSpeed = currentSpeed + 1}})
                                    where currentSpeed = gameSpeed $ settings gs
eventSettingMenuSpeed _ gs = return gs
