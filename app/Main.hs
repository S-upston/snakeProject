module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)
import Data.Maybe (fromJust, maybeToList)
import System.Random (randomRs, mkStdGen)

-- Data Types
data GameState = GameState
  { snake        :: [(Int, Int)]
  , secondSnake  :: Maybe [(Int, Int)]
  , items        :: [(Int, Int)]
  , walls        :: [(Int, Int)]
  , score        :: Int
  , hiScore      :: Int
  , isGameOver   :: Bool
  , isWin        :: Bool
  , inTailMode   :: Bool
  , inDuoMode    :: Bool
  , screen       :: Screen
  , direction    :: Direction
  , leaderboard  :: [Int]
  }
  deriving (Show)

data Screen = Start | Game | GameOver | Leaderboard deriving (Eq, Show)
data Direction = U | D | L | R deriving (Eq, Show)

-- Constants
windowWidth, windowHeight, cellSize :: Int
windowWidth = 640
windowHeight = 640
cellSize = 20

gridSize :: Int
gridSize = 21

-- Levels
level1Walls, level2Walls, level3Walls :: [(Int, Int)]
level1Walls = [(x, 0) | x <- [0..20]] ++
              [(x, 20) | x <- [0..20]] ++
              [(0, y) | y <- [0..20]] ++
              [(20, y) | y <- [0..20]]

level2Walls = level1Walls ++
              [(x, 10) | x <- [8..12]] ++
              [(x, 11) | x <- [8..12]]

level3Walls = level1Walls ++
              [(x, 5) | x <- [5..15]] ++
              [(x, 15) | x <- [5..15]] ++
              [(5, y) | y <- [6..14]] ++
              [(15, y) | y <- [6..14]]

-- Initial State
initialState :: GameState
initialState = GameState
  { snake = [(10, 10), (10, 9), (10, 8), (10, 7)]
  , secondSnake = Nothing
  , items = [(5, 5)]
  , walls = []
  , score = 0
  , hiScore = 0
  , isGameOver = False
  , isWin = False
  , inTailMode = False
  , inDuoMode = False
  , screen = Start
  , direction = R
  , leaderboard = []
  }

-- Main
main :: IO ()
main = play
  (InWindow "Snake Game" (windowWidth, windowHeight) (100, 100))
  black
  10
  initialState
  drawGameState
  handleInput
  updateGameState

-- Drawing
drawGameState :: GameState -> Picture
drawGameState state = case screen state of
  Start       -> drawStartScreen state
  Game        -> drawGame state
  GameOver    -> drawGameOver state
  Leaderboard -> drawLeaderboard state

drawStartScreen :: GameState -> Picture
drawStartScreen _ = Pictures
  [ Translate (-200) 100 $ Scale 0.3 0.3 $ Text "Snake Game"
  , Translate (-200) 50 $ Scale 0.2 0.2 $ Text "Press 1 for Level 1"
  , Translate (-200) 20 $ Scale 0.2 0.2 $ Text "Press 2 for Level 2"
  , Translate (-200) (-10) $ Scale 0.2 0.2 $ Text "Press 3 for Level 3"
  , Translate (-200) (-50) $ Scale 0.2 0.2 $ Text "Press F1 for Tail Mode"
  , Translate (-200) (-80) $ Scale 0.2 0.2 $ Text "Press F2 for Duo Mode"
  , Translate (-200) (-120) $ Scale 0.2 0.2 $ Text "Press Enter to Start"
  ]

drawGame :: GameState -> Picture
drawGame state =
  Pictures
    [ drawSnake (snake state)
    , maybe Blank drawSnake (secondSnake state)
    , drawItems (items state)
    , drawWalls (walls state)
    , drawScore state -- Pass GameState
    ]


drawGameOver :: GameState -> Picture
drawGameOver state = Pictures
  [ Translate (-200) 100 $ Scale 0.3 0.3 $ Text "Game Over!"
  , Translate (-200) 50 $ Scale 0.2 0.2 $ Text $ "Score: " ++ show (score state)
  , Translate (-200) 20 $ Scale 0.2 0.2 $ Text "Press R to Restart"
  , Translate (-200) (-10) $ Scale 0.2 0.2 $ Text "Press L to View Leaderboard"
  ]

drawLeaderboard :: GameState -> Picture
drawLeaderboard state = Pictures
  [ Translate (-200) 100 $ Scale 0.3 0.3 $ Text "Leaderboard"
  , Translate (-200) 50 $ Scale 0.2 0.2 $ Text $ unlines (map show $ leaderboard state)
  , Translate (-200) (-50) $ Scale 0.2 0.2 $ Text "Press Enter to Return"
  ]

drawSnake, drawItems, drawWalls, drawScore :: [(Int, Int)] -> Picture
drawSnake s = Pictures $ map drawCell s
drawItems i = Pictures $ map drawCell i
drawWalls w = Pictures $ map drawCell w

drawCell :: (Int, Int) -> Picture
drawCell (x, y) =
  Translate (fromIntegral x * fromIntegral cellSize) (fromIntegral y * fromIntegral cellSize) $
  Color white $
  rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)


drawScore :: GameState -> Picture
drawScore state =
  Translate (-300) 300 $ Scale 0.2 0.2 $ Text $ "Score: " ++ show (score state)


-- Input Handling
handleInput :: Event -> GameState -> GameState
handleInput event state = case screen state of
  Start       -> handleStartScreen event state
  Game        -> handleGameInput event state
  GameOver    -> handleGameOverInput event state
  Leaderboard -> handleLeaderboardInput event state

handleStartScreen :: Event -> GameState -> GameState
handleStartScreen (EventKey (SpecialKey KeyF1) Down _ _) state =
  state { inTailMode = not (inTailMode state) } -- Toggle Tail Mode
handleStartScreen (EventKey (SpecialKey KeyF2) Down _ _) state =
  state { inDuoMode = not (inDuoMode state) } -- Toggle Duo Mode
handleStartScreen (EventKey (Char '1') Down _ _) state =
  state { walls = level1Walls } -- Set Level 1
handleStartScreen (EventKey (Char '2') Down _ _) state =
  state { walls = level2Walls } -- Set Level 2
handleStartScreen (EventKey (Char '3') Down _ _) state =
  state { walls = level3Walls } -- Set Level 3
handleStartScreen (EventKey (SpecialKey KeyEnter) Down _ _) state =
  state { screen = Game } -- Start the game
handleStartScreen _ state = state


handleGameInput :: Event -> GameState -> GameState
handleGameInput (EventKey (SpecialKey KeyUp) Down _ _) state
  | direction state /= D = state { direction = U }
handleGameInput (EventKey (SpecialKey KeyDown) Down _ _) state
  | direction state /= U = state { direction = D }
handleGameInput (EventKey (SpecialKey KeyLeft) Down _ _) state
  | direction state /= R = state { direction = L }
handleGameInput (EventKey (SpecialKey KeyRight) Down _ _) state
  | direction state /= L = state { direction = R }
handleGameInput _ state = state

handleGameOverInput :: Event -> GameState -> GameState
handleGameOverInput (EventKey (Char 'r') Down _ _) state =
  initialState { hiScore = max (hiScore state) (score state) } -- Restart game
handleGameOverInput (EventKey (Char 'l') Down _ _) state =
  state { screen = Leaderboard } -- Go to leaderboard
handleGameOverInput _ state = state

handleLeaderboardInput :: Event -> GameState -> GameState
handleLeaderboardInput (EventKey (SpecialKey KeyEnter) Down _ _) state =
  state { screen = Start }
handleLeaderboardInput _ state = state

-- Update Logic
updateGameState :: Float -> GameState -> GameState
updateGameState _ state
  | isGameOver state || isWin state = state -- No updates if the game is over or won
  | otherwise =
      let newSnake = moveSnake (snake state) (direction state)
          secondSnakeUpdated = if inDuoMode state
                               then Just (moveSnake (fromJust $ secondSnake state) (direction state))
                               else Nothing
          collision = checkCollision newSnake (walls state ++ snake state)
          itemEaten = any (== head newSnake) (items state)
          newItems = if itemEaten
                     then spawnNewItem (walls state ++ newSnake ++ concat (maybeToList secondSnakeUpdated))
                     else items state
          updatedSnake = if itemEaten
                         then growSnake newSnake
                         else tail newSnake
          updatedSecondSnake = if itemEaten && inDuoMode state
                               then Just (growSnake $ fromJust secondSnakeUpdated)
                               else secondSnakeUpdated
          winCondition = length updatedSnake == gridSize ^ 2 - length (walls state)
      in state
         { snake = if inTailMode state then tailToHead updatedSnake else updatedSnake
         , secondSnake = updatedSecondSnake
         , items = newItems
         , score = if itemEaten then score state + 1 else score state
         , isGameOver = collision
         , isWin = winCondition
         }
         
moveSnake :: [(Int, Int)] -> Direction -> [(Int, Int)]
moveSnake ((x, y):xs) U = (x, y + 1) : init ((x, y):xs)
moveSnake ((x, y):xs) D = (x, y - 1) : init ((x, y):xs)
moveSnake ((x, y):xs) L = (x - 1, y) : init ((x, y):xs)
moveSnake ((x, y):xs) R = (x + 1, y) : init ((x, y):xs)
moveSnake [] _ = []

checkCollision :: [(Int, Int)] -> [(Int, Int)] -> Bool
checkCollision (head:_) walls = head `elem` walls
checkCollision _ _ = False

growSnake :: [(Int, Int)] -> [(Int, Int)]
growSnake snake = snake ++ [last snake]

tailToHead :: [(Int, Int)] -> [(Int, Int)]
tailToHead snake = last snake : init snake

spawnNewItem :: [(Int, Int)] -> [(Int, Int)]
spawnNewItem occupied = 
  let available = [(x, y) | x <- [1..gridSize - 1], y <- [1..gridSize - 1], (x, y) `notElem` occupied]
  in if null available
     then []
     else [available !! head (randomRs (0, length available - 1) (mkStdGen 42))]



