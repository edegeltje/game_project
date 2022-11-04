{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Controller.GameController where
import Model
import Model.Menus
import Model.Entities
import Controller.MenuController
import View.StaticSprites
import Model.Settings (tILESIZE)

inputFromButton :: GameState -> InputButton -> IO GameState
inputFromButton gs ib = return gs {inputBuffer = ib}

step :: GameState -> Float -> IO GameState
step gs@MkGameState{inputBuffer = InputNeutral} t = return gs {time = time gs + t}
step gs@MkGameState{inputBuffer = InputBack} t = return gs {inputBuffer = InputNeutral, time = time gs + t, menuState = PauseMenu ContinueOption}
step gs@MkGameState{entities = currentEntities@MkEntityRecord{player = currentPlayer}} t 
    | dirPossible gs (getPosition currentPlayer) (inputToDir (inputBuffer gs)) = return gs {inputBuffer = InputNeutral, time = time gs + t,  entities = currentEntities{player = currentPlayer{playerPosition = calcNewPosition (playerPosition currentPlayer) (inputToDir (inputBuffer gs)), playerMovementDirection = inputToDir (inputBuffer gs)}}} --This makes pacman move in the direction in the inputbuffer if it is possible
    | dirPossible gs (getPosition currentPlayer) (getDirection currentPlayer)  = return gs {time = time gs + t, entities = currentEntities{player = currentPlayer{playerPosition = calcNewPosition (playerPosition currentPlayer) (playerMovementDirection currentPlayer)}}} --This makes pacman move in the current direction if the direction in the input buffer is not possible
    | otherwise                                                                = return gs {time = time gs + t} --This makes sure that pacman stops moving when he hits a wall
step gs t                                                                      = return gs {time = time gs + t}

inputToDir :: InputButton -> Direction
inputToDir InputUp = North
inputToDir InputRight = East
inputToDir InputLeft = West
inputToDir InputDown = South

calcNewPosition :: Position -> Direction -> Position
calcNewPosition p@MkPosition{yposition = yPos} North = p{yposition = yPos + tILESIZEInt}
calcNewPosition p@MkPosition{xposition = xPos} East = p{xposition = xPos + tILESIZEInt}
calcNewPosition p@MkPosition{yposition = yPos} South = p{yposition = yPos - tILESIZEInt}
calcNewPosition p@MkPosition{xposition = xPos} West = p{xposition = xPos - tILESIZEInt}

dirPossible :: GameState -> Position -> Direction -> Bool
dirPossible gs pos dir = getPositionContent (calcNewPosition pos dir) (maze gs) /= Wall

tILESIZEInt :: Int
tILESIZEInt = round tILESIZE