module Model.Settings where
data Settings = MkSettings{
  volume :: Int,
  gameSpeed :: Float -- how many tiles can be traversed in a single second?
}
  deriving Show
  -- something something sound volume?
  -- something something keybindings?
  -- something something fullscreen?

tILESIZE = 8 :: Float