module Model.Menus where
import Model.Settings (Settings (volume, gameSpeed))

data MenuState = PauseMenu PauseMenuState | StartMenu StartMenuState | Playing
  deriving (Show)
data PauseMenuState = ContinueOption | PauseSettingOption SettingMenuState | ExitToStartOption | ExitToDesktopOption
  deriving (Show, Eq)
data StartMenuState = PlayOption | StartSettingOption SettingMenuState| LoadOption | ExitOption
  deriving (Show, Eq)
data SettingMenuState = VolumeOption | SpeedOption | SuperMenu
  deriving (Show, Eq)

class Eq a => MenuOption a where
  optionsWithNames :: a-> [(a,String)]
  options :: a -> [a]
  menuName :: a -> String
  options = map fst . optionsWithNames

instance MenuOption PauseMenuState where
  optionsWithNames = const [
    (ContinueOption, "Continue"),
    (PauseSettingOption SuperMenu, "Settings"),
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


