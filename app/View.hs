module View where

import Graphics.Gloss

window :: Display
window = InWindow "Pacman" (800, 600) (10, 10) 

pacmanClosed :: Picture
pacmanClosed = pictures [color yellow (circleSolid 80), translate 25 25 (color black (circleSolid 10))]

pacmanOpenMouth :: Float -> Picture
pacmanOpenMouth time = rotate 180 (pictures [color yellow (circleSolid 80), arcSolid (-abs(sin(180 + 15 * time))) (abs(sin(180 - 15 * time))) 80])

animatePacman :: IO ()
animatePacman = animate window black pacmanOpenMouth

test :: IO ()
test =  display (InWindow "example" (800, 600) (0, 0)) black (color green (circle 100))

baz :: IO()
baz = putStrLn "baz function in View"