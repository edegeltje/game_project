
{-# LANGUAGE DeriveGeneric #-}
module Model.Menus where
import Model.Settings (Settings (volume, gameSpeed))
import GHC.Generics

data MenuState = PauseMenu PauseMenuState | StartMenu StartMenuState | Playing
  deriving (Show, Generic)
data PauseMenuState = ContinueOption | PauseSettingOption SettingMenuState | SaveOption | ExitToStartOption | ExitToDesktopOption
  deriving (Show, Eq, Generic)
data StartMenuState = PlayOption | StartSettingOption SettingMenuState| LoadOption | LoadGameOption| ExitOption
  deriving (Show, Eq, Generic)
data SettingMenuState = VolumeOption | SpeedOption | SuperMenu
  deriving (Show, Eq, Generic)

class Eq a => MenuOption a where
  optionsWithNames :: a-> [(a,String)]
  options :: a -> [a]
  menuName :: a -> String
  options = map fst . optionsWithNames

instance MenuOption PauseMenuState where
  optionsWithNames = const [
    (ContinueOption, "Continue"),
    (PauseSettingOption SuperMenu, "Settings"),
    (SaveOption, "Save Game"),
    (ExitToStartOption, "Exit To Start"),
    (ExitToDesktopOption, "Exit To Desktop")
    ]
  menuName (PauseSettingOption SuperMenu) = "Pause"
  menuName (PauseSettingOption _) = "Settings"
  menuName _ = "Pause"

instance MenuOption StartMenuState where
  optionsWithNames = const [
    (PlayOption,"Play"),
    (StartSettingOption SuperMenu, "Settings"),
    (LoadOption, "Load level"),
    (LoadGameOption, "Load Saved Game"),
    (ExitOption, "Exit")
    ]
  menuName (StartSettingOption SuperMenu) = "Start"
  menuName (StartSettingOption _) = "Settings"
  menuName _ = "Start"

instance MenuOption SettingMenuState where
  optionsWithNames = const [
    (VolumeOption, "Volume"),
    (SpeedOption, "GameSpeed")
    ]
  menuName _ = "Settings"

getSettingOptions :: Settings -> [(SettingMenuState, String)]
getSettingOptions s = [
  (VolumeOption, "Volume  "++ show (volume s)),
  (SpeedOption, "Speed   "++ show (gameSpeed s))
  ]


data AnimationState = WinScreen Float | NoAnimation | GameOver Float
  deriving (Show, Generic)