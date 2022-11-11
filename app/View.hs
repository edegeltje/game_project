module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Model
import Model.Entities
import Model.Menus
import Model.Settings
import qualified Data.Map as DM
import View.AnimatedSprites
import View.StaticSprites (window, fruitSpritesIO)

import qualified View.ViewGame as VG
import qualified View.ViewMenu as VM
import System.Random (mkStdGen)

animatePacTest :: IO ()
animatePacTest = animate window black (animatePacman testPlayer)

test :: IO ()
test =  display (InWindow "example" (800, 600) (0, 0)) black (color green (circle 100))

baz :: IO()
baz = putStrLn "baz function in View"

view :: FruitSprites -> GameState -> Picture
view fs gs@MkGameState {menuState = Playing} = VG.view fs gs
view fs gs = pictures [VM.view gs
  ,color white $ translate' (-30,-10) $ scale 0.2 0.2 $ text $ show $ inputBuffer gs,
  color white $ translate' (-30,-30) $ scale 0.2 0.2 $ text $ show $ menuState gs
  ]


testPlayer :: PlayerEntity
testPlayer = MkPlayer 
      (0,0) 
      South 
      Weak
      1

testGameState :: GameState
testGameState = MkGameState 
  Playing 
  DM.empty 
  0 
  1 
  (MkEntityRecord 
    testPlayer
    []
    []
    Chase)
  InputNeutral 
  0
  (MkSettings 0 0)
  testRngSeed



--borderTuples :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
--borderTuples (startX, startY)  (endX, endY) =  [(x,startY) | x <- [startX..endX]] ++ [(x, endY) | x <- [startX..endX]] ++ [(startX, y) | y <- [startY..endY]] ++ [(endX, y) | y <- [startY..endY]]



--borderList :: [((Int, Int),BottomLayerContent)]
--borderList = map (\x -> (x, Wall)) (borderTuples (-2,-2) (20,20))

-- alsoNothingList :: (Int, Int) -> (Int, Int) -> [((Int, Int), BottomLayerContent)] -> [((Int, Int), BottomLayerContent)]
-- alsoNothingList (startX, startY) (endX, endY) maze = [((x,y), Empty) | (x,y) <-  [not (`elem` map fst maze) (x,y) | x<- [startX..endX], y <- [startY..endY]]]

--testList :: [((Int, Int),BottomLayerContent)]
--testList = ((-100, -100), SmallDot) : alsoNothingList (-2, -2) (20, 20) borderList

buildWall :: (Int, Int) -> (Int, Int) -> [((Int, Int), BottomLayerContent)]
buildWall (startX, startY) (endX, endY) = [((x,y), Wall) | x <- [startX..endX], y <- [startY..endY]]

tuples = [(x,y) | x<- [-25..25], y <- [-25..25]]
contentCalc :: (Int,Int) -> BottomLayerContent
contentCalc (-2,_)  = Wall
contentCalc (20,_) = Wall
contentCalc (_,-2)  = Wall
contentCalc (_,20) = Wall
contentCalc (_,1)  = SmallDot
contentCalc (_,2)  = PowerDot
contentCalc _      = Empty
content = map contentCalc tuples

addEmpty :: [((Int, Int), BottomLayerContent)] -> [(Int, Int)] -> [((Int, Int), BottomLayerContent)]
addEmpty content []                                = content
addEmpty content (x:xs) | x `elem` map fst content = addEmpty content xs
                        | otherwise                = (x, Empty) : addEmpty content xs

testMaze = DM.fromList $ zip tuples content :: BottomLayer

buildBorder :: Int -> [((Int, Int), BottomLayerContent)]
buildBorder r = concatMap (uncurry buildWall) [((-r, -r), (-r, r)), ((r, -r), (r, r)), ((-r, -r), (r, -r)),((-r, r), (r, r))]

level1Walls :: [((Int, Int), (Int, Int))]
level1Walls = [((-16, -18), (-12, -12)), ((12, 12), (16, 18)), ((-16, 12), (-12, 18)), ((12, -18), (16, -12)), ((-9, -16), (9, -12)), ((-9, 12), (9, 16)), 
               ((-16, -23), (16, -18)), ((-16, 18), (16,23)), ((-23, -23), (-19, -18)), ((19, -23), (23,-18)), ((19, 18), (23,23)), ((-23, 18), (-19,23)),
               ((-1, -8), (1, 8)), ((4,6), (10,8)), ((1, -1), (10, 1)), ((4, -8), (10, -6)), ((12,2), (22,5)), ((12, -5), (22, -2)),
               ((-10, 6), (-4, 8)), ((-10, -1), (-1, 1)), ((-10, -8), (-4, -6)), ((-22, 2), (-12, 5)), ((-22, -5), (-12, -2)),
               ((-23, -14), (-19, -9)), ((19, -14), (23,-9)), ((19, 9), (23,14)), ((-23, 9), (-19,14))]

level1SmallDots :: [(Int, Int)]
level1SmallDots = [(x,24) | x <- [-23..23], not (x == 0)] ++ [(x,-24) | x <- [-23..23], not (x == 0)] ++ [(x,17) | x <- [-11..11], not (x == 0)] ++ [(x,-17) | x <- [-11..11], not (x == 0)]
                  ++ [(x,y) | x <- [12..22], y <- [-1..1]] ++ [(x,y) | x <- [-22..(-12)], y <- [-1..1]]

level1PowerDots :: [(Int, Int)]
level1PowerDots = [(-24,24), (0,24), (24,24), (-24,-24), (0,-24), (24,-24), (0,17), (0, -17)]

buildLevel1Walls :: [((Int, Int), BottomLayerContent)]
buildLevel1Walls = concatMap (uncurry buildWall) level1Walls

placeLevel1SmallDots :: [((Int, Int), BottomLayerContent)]
placeLevel1SmallDots = map (\(x,y) -> ((x,y), SmallDot)) level1SmallDots

placeLevel1PowerDots :: [((Int, Int), BottomLayerContent)]
placeLevel1PowerDots = map (\(x,y) -> ((x,y), PowerDot)) level1PowerDots

level1Fruits :: [Fruit]
level1Fruits = [
  MkFruit Cherry     (-2,2)  200,
  MkFruit Bell       (-2,-2)  200,
  MkFruit Apple      (2,-2)  200,
  MkFruit Galaxian   (-24,-14)  200,
  MkFruit Key        (-24,14) 200,
  MkFruit Melon      (24,-14) 200,
  MkFruit Orange     (24,14) 200,
  MkFruit Strawberry (-18,-7) 200]

level1Enemies :: [EnemyEntity]
level1Enemies = [
  MkEnemy (-20,16) (0,0) North Inky Alive 1,
  MkEnemy (20,16) (0,0) East Pinky Alive 1,
  MkEnemy (-20,-16) (0,0) West Blinky Alive 1,
  MkEnemy (20,-16) (0,0) South Clyde Alive 1
  ]


testMaze' = DM.fromList $ addEmpty (((buildBorder 25) ++ buildLevel1Walls) ++ placeLevel1PowerDots ++ placeLevel1SmallDots) tuples

level1Maze = DM.fromList $ addEmpty (((buildBorder 25) ++ buildLevel1Walls) ++ placeLevel1PowerDots ++ placeLevel1SmallDots) tuples

testEnemies :: [EnemyEntity]
testEnemies = [
  MkEnemy (2,1) (0,0) North Inky Alive 1,
  MkEnemy (2,3) (0,0) East Pinky Alive 1,
  MkEnemy (4,1) (0,0) West Blinky Alive 1,
  MkEnemy (4,3) (0,0) South Clyde Alive 1
  ]

testFruits :: [Fruit]
testFruits = [
  MkFruit Cherry     (7,1)  60,
  MkFruit Bell       (7,3)  60,
  MkFruit Apple      (9,1)  60,
  MkFruit Galaxian   (9,3)  60,
  MkFruit Key        (11,1) 60,
  MkFruit Melon      (11,3) 60,
  MkFruit Orange     (13,1) 60,
  MkFruit Strawberry (13,3) 60]

testPlayer' :: PlayerEntity
testPlayer' = MkPlayer (15,1) West Weak 1

level1Player :: PlayerEntity
level1Player = MkPlayer (15,1) West Weak 1

level1Entities :: EntityRecord
level1Entities = MkEntityRecord level1Player level1Enemies level1Fruits Scatter

testEntities :: EntityRecord
testEntities = MkEntityRecord testPlayer' testEnemies testFruits Scatter

level1GameState :: GameState
level1GameState = 
  MkGameState
    Playing level1Maze 1 2 level1Entities InputNeutral 0 (MkSettings 1 8) testRngSeed

calcGameStateLevel1 :: Float -> GameState
calcGameStateLevel1 t = MkGameState
    Playing level1Maze 1 2 level1Entities InputNeutral t (MkSettings 1 8) testRngSeed

level1View = do
  fs <- fruitSpritesIO
  animateIO window black
    (return . view fs . calcGameStateLevel1) (const $ return ())


testGameState' :: GameState
testGameState' = 
  MkGameState
    Playing testMaze' 1 2 testEntities InputNeutral 0 (MkSettings 1 1) testRngSeed

calcGameState :: Float -> GameState
calcGameState t = MkGameState
    Playing testMaze' 1 2 testEntities InputNeutral t (MkSettings 1 1) testRngSeed

testView = do
  fs <- fruitSpritesIO
  animateIO window black
    (return . view fs . calcGameState) (const $ return ())