module Controller.GameController where
import Model
import Model.Menus
import Model.Entities
import Controller.MenuController
import View.StaticSprites
import Model.Settings (tILESIZE, Settings (gameSpeed))
import Data.Map

inputFromButton :: GameState -> InputButton -> IO GameState
inputFromButton gs ib = return gs {inputBuffer = ib}

newTime :: GameState -> Float -> Float
newTime gs dt = time gs + dt * gameSpeed (settings gs)

step :: GameState -> Float -> IO GameState
step gs@MkGameState{inputBuffer = InputNeutral} dt | floor (time gs) == floor (newTime gs dt) =
  return gs {time = newTime gs dt, entities = newEntities, maze = newMaze, score = score gs + scoreD1 + scoreD2}-- don't update the model every frame, just every "second"
  where
      (newEntities,scoreD1) = updateEntities gs
      (newMaze,scoreD2) = updateMaze gs newEntities
step gs@MkGameState{inputBuffer = InputBack} dt = return
  gs {
    inputBuffer = InputNeutral,
    time = newTime gs dt,
    menuState = PauseMenu ContinueOption
    }
step gs@MkGameState{ entities = currentEntities@MkEntityRecord{player = currentPlayer}} dt
    | dirPossible gs (getPosition currentPlayer) (inputToDir (inputBuffer gs))
      = return gs {
        time = newTime gs dt,
        entities = newEntities{player = currentPlayer{
            playerPosition = calcNewPosition
              (playerPosition currentPlayer)
              (inputToDir (inputBuffer gs)),
            playerMovementDirection = inputToDir (inputBuffer gs)
            }},
        maze = newMaze,
        score = score gs + scoreD1 + scoreD2
        } --This makes pacman move in the direction in the inputbuffer if it is possible
    | dirPossible gs (getPosition currentPlayer) (getDirection currentPlayer)  = return
      gs {
        time = newTime gs dt,
        entities = newEntities{
          player = currentPlayer{
            playerPosition = calcNewPosition
              (playerPosition currentPlayer)
              (playerMovementDirection currentPlayer)
            }
          },
        maze = newMaze,
        score = score gs + scoreD1 + scoreD2
        } --This makes pacman move in the current direction if the direction in the input buffer is not possible
    | otherwise = return gs {time = newTime gs dt,
    entities = newEntities,
    maze = newMaze,
    score = score gs + scoreD1 + scoreD2}
    --This makes sure that pacman stops moving when he hits a wall
    where
      (newEntities,scoreD1) = updateEntities gs
      (newMaze,scoreD2) = updateMaze gs newEntities

-- step gs dt =
--   return gs{
--     entities = newEntities, 
--     maze = newMaze, 
--     time = newTime gs dt,
--     score = score gs + scoreD1 + scoreD2}
--       where
--         (newEntities,scoreD1) = updateEntities gs
--         (newMaze,scoreD2) = updateMaze gs newEntities

{- 
once every in-game second, we will calculate what happens the next.
since pacman travels at (less than) one tile per in-game second,
 this allows us to only have to calculate one "decision" for pacman per update.
the update consists of two parts: 
1. updating the model of where everything is
2. calculating what will happen in the next in-game second.

part 1 includes setting the new (immediate) positions, states and directions 
 for the ghosts and pacman.
it also includes removing any collected fruits and pellets from the map.
part two includes calculating the decisions for the ghosts in the next in-game seconds.
the reason why this is necessary is because,
 although we know that pacman moves at integer tile-per-second,
 we don't know that about the ghosts, and as a result, they can be between two tiles at the time
 of updating.

-}


step gs dt = return gs {time = newTime gs dt}

inputToDir :: InputButton -> Direction
inputToDir InputUp = North
inputToDir InputRight = East
inputToDir InputLeft = West
inputToDir InputDown = South
inputToDir _ = North


updateEntities :: GameState -> (EntityRecord,Score)
updateEntities gs@MkGameState{entities = currentEntities@MkEntityRecord{player = currentPlayer}} 
              | getPositionContent (playerPosition currentPlayer) (maze gs) == PowerDot 
                = ((entities gs){enemies = Prelude.map scare (enemies (entities gs))}, currentScore)
  where
    scare enemies = enemies{enemyStatus = Scared}
    currentScore = score gs
    
updateEntities gs = (entities gs, score gs)

updateMaze :: GameState -> EntityRecord -> (BottomLayer,Score)
updateMaze gs er@MkEntityRecord{player = currentPlayer} 
          | getPositionContent (playerPosition currentPlayer) (maze gs) == PowerDot 
            = (insert (playerPosition currentPlayer) Empty (maze gs) , currentScore + 50)
  where
    currentScore = score gs

updateMaze gs er@MkEntityRecord{player = currentPlayer} 
          | getPositionContent (playerPosition currentPlayer) (maze gs) == SmallDot
            = (insert (playerPosition currentPlayer) Empty (maze gs) , currentScore + 10)
  where
    currentScore = score gs

updateMaze gs er = (maze gs, score gs)

calcNewPosition :: Position -> Direction -> Position
calcNewPosition (x,y) North = (x,y + 1)
calcNewPosition (x,y) East = (x+ 1, y)
calcNewPosition (x,y) South = (x,y - 1)
calcNewPosition (x,y) West = (x - 1,y)

dirPossible :: GameState -> Position -> Direction -> Bool
dirPossible gs pos dir = getPositionContent (calcNewPosition pos dir) (maze gs) /= Wall
