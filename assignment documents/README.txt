-- Starting the game

The game is started by running "cabal build" and then "cabal run" in the "game_project" folder.

-- Controls

Everywhere where the arrow keys can be used, the "wasd" keys can be used as well.

--Menus
The menus are controlled by the arrow keys. When there is a number to adjust, you can press left or right arrow key to adjust said number.
When in the settings menu, you can return to the previous menu by pressing backspace or escape.
To select an option, press enter.

--Playing
Use the arrow keys to control pacman. Backspace or escape can be pressed to enter the pause menu.


-- Playing the game

-- Start menu

When the game starts you find yourself in the starting menu. There are a few things you can do from here. 

Play:
Selecting Play will start a new version of level 1.

Settings:
Here you can adjust the setting. We unfortunately could not implement sound in our game, so only adjusting gameSpeed wil change the game.

Load Level:
Choose a level to play with the left and right arrow key. Press enter to play the selected level. You can choose between level 1, level 2 or level 3.

Load Saved Game:
If you have saved your progress during a game, you can load that gamestate again using this option.

Exit:
Closes the game

-- Playing

The goal of the game is to collect all small dots and powerdots. 

-- Pause menu

Continue:
Continue playing the game you were playing

Settings:
Brings you to the settings menu. Here you can press escape or backspace to return to the pause menu

Save Game:
This will save the current gameState. This can be loaded back in the game at any point from the start menu, even if the game was closed in the meantime.

Exit to Start:
Returns to the start menu

Exit to Desktop:
Closes the game

