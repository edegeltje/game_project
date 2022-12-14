module Main where

import Graphics.Gloss.Interface.IO.Game

import Model

import Controller
import Controller.JsonInteract

import View
import View.StaticSprites (window, fruitSpritesIO)
import View.AnimatedSprites (renderTestGhost)

main :: IO ()
main = do fruitSprites <- fruitSpritesIO
          playIO window black 60 startGameState (return . view fruitSprites) input step
          --level <- loadLevel 1
          --playIO window black 60 (levelToGameState level) (return . view fruitSprites) input step
          --saveGameStatetest
          --saveLevel