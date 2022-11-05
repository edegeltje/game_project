module Controller where

import Graphics.Gloss.Interface.IO.Game

import Model
import Model.Entities
import Model.Menus

import qualified Controller.MenuController as MC
import qualified Controller.GameController as GC

bar :: IO ()
bar = putStrLn "bar function in Controller"

step :: Float -> GameState -> IO GameState
step = flip step'

step' :: GameState -> Float -> IO GameState
step' gs@MkGameState {menuState = Playing} = GC.step gs
step' gs = const $ return gs

input :: Event -> GameState -> IO GameState

input (EventKey key Down _ _) = flip inputFromButton $ keyToInput key
input _ = return

keyToInput :: Key -> InputButton
keyToInput key = case key of
  Char       'a'          -> InputLeft
  Char       'A'          -> InputLeft
  SpecialKey KeyLeft      -> InputLeft
  Char       's'          -> InputDown
  Char       'S'          -> InputDown
  SpecialKey KeyDown      -> InputDown
  Char       'w'          -> InputUp
  Char       'W'          -> InputUp
  SpecialKey KeyUp        -> InputUp
  Char       'd'          -> InputRight
  Char       'D'          -> InputRight
  SpecialKey KeyRight     -> InputRight
  SpecialKey KeyBackspace -> InputBack
  SpecialKey KeyEsc       -> InputBack
  SpecialKey KeyEnter     -> InputSelect
  SpecialKey KeyPadEnter  -> InputSelect
  _                       -> InputNeutral

inputFromButton :: GameState -> InputButton -> IO GameState
inputFromButton gs@MkGameState {menuState = Playing} inputb= GC.inputFromButton gs inputb
inputFromButton gs inputb= MC.inputFromButton (gs{inputBuffer = inputb}) inputb