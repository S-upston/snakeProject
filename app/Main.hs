import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)
import Data.List (insert)

-- Constants
windowWidth, windowHeight, blockSize :: Int
windowWidth = 420
windowHeight = 420
blockSize = 20

-- Types
data Direction = U | D | L | R deriving (Eq)
type Position = (Int, Int)

data GameState = GameState
  { snake      :: [Position]
  , dir        :: Direction
  , food       :: Position
  , alive      :: Bool
  , score      :: Int
  , hiScore    :: Int
  , screen     :: Screen
  , walls      :: [Position]
  , tailMode   :: Bool
  , duoMode    :: Bool
  , duoSnake   :: [Position]
  , leaderboard :: [Int]
  }

data Screen = Start | Game | GameOver | Leaderboard deriving (Eq)

-- Convert game coordinates to Gloss coordinates
toGlossCoord :: Position -> (Float, Float)
toGlossCoord (x, y) = (fromIntegral x * fromIntegral blockSize, fromIntegral y * fromIntegral blockSize)

-- Rendering functions
render :: GameState -> Picture
render gameState = case screen gameState of
  Start        -> renderStartScreen
  Game         -> renderGameScreen gameState
  GameOver     -> renderGameOverScreen gameState
  Leaderboard  -> renderLeaderboardScreen (leaderboard gameState)

renderStartScreen :: Picture
renderStartScreen = pictures
  [ translate (-130) 100 (scale 0.2 0.2 (color white (text "Press ENTER to Start")))
  , translate (-120) 50 (scale 0.2 0.2 (color white (text "Use ARROW KEYS to move")))
  , translate (-120) 0 (scale 0.2 0.2 (color white (text "Press T to toggle Tail Mode")))
  , translate (-120) (-50) (scale 0.2 0.2 (color white (text "Press D to toggle Duo Mode")))
  , translate (-120) (-100) (scale 0.2 0.2 (color white (text "Press L to change level")))
  ]

renderGameScreen :: GameState -> Picture
renderGameScreen (GameState snake _ food _ score hiScore walls _ _ duoSnake) = pictures $
  [ translateBlock pos (color green (rectangleSolid size size)) | pos <- snake ] ++
  [ translateBlock pos (color blue (rectangleSolid size size)) | pos <- duoSnake ] ++
  [ translateBlock pos (color white (rectangleSolid size size)) | pos <- walls ] ++
  [ translateBlock food (color red (rectangleSolid size size)) ] ++
  [ translate (-fromIntegral windowWidth / 2 + 10) (fromIntegral windowHeight / 2 - 30)
      (scale 0.1 0.1 (color white (text $ "Score: " ++ show score ++ " Hi-Score: " ++ show hiScore))) ]
  where
    translateBlock (x, y) block = translate (fromIntegral x * fromIntegral blockSize) (fromIntegral y * fromIntegral blockSize) block
    size = fromIntegral blockSize

renderGameOverScreen :: GameState -> Picture
renderGameOverScreen gameState = pictures
  [ translate (-110) 50 (scale 0.3 0.3 (color white (text "Game Over")))
  , translate (-150) (-50) (scale 0.2 0.2 (color white (text $ "Final Score: " ++ show (score gameState))))
  , translate (-150) (-100) (scale 0.2 0.2 (color white (text "Press ENTER to Restart or L for Leaderboard")))
  ]

renderLeaderboardScreen :: [Int] -> Picture
renderLeaderboardScreen scores = pictures $
  [ translate (-130) 100 (scale 0.3 0.3 (color white (text "Leaderboard"))) ] ++
  zipWith (\y score -> translate (-100) y (scale 0.2 0.2 (color white (text $ show score)))) [50, 30..] (take 10 scores)

-- Initialization
initialState :: IO GameState
initialState = do
  foodPos <- randomFoodPosition
  return GameState
    { snake = [(0, 0), (-1, 0), (-2, 0), (-3, 0)]
    , dir = R
    , food = foodPos
    , alive = True
    , score = 0
    , hiScore = 0
    , screen = Start
    , walls = []
    , tailMode = False
    , duoMode = False
    , duoSnake = []
    , leaderboard = []
    }

randomFoodPosition :: IO Position
randomFoodPosition = do
  x <- randomRIO (-w, w)
  y <- randomRIO (-h, h)
  return (x, y)
  where
    w = windowWidth `div` (2 * blockSize) - 1
    h = windowHeight `div` (2 * blockSize) - 1

-- Update logic
update :: Float -> GameState -> IO GameState
update _ gameState = case screen gameState of
  Start        -> return gameState
  Game         -> updateGame gameState
  GameOver     -> return gameState
  Leaderboard  -> return gameState

updateGame :: GameState -> IO GameState
updateGame gameState
  | not (alive gameState) = return gameState { screen = GameOver, leaderboard = updateLeaderboard (score gameState) (leaderboard gameState) }
  | otherwise = do
      let newHead = move (dir gameState) (head (snake gameState))
          newSnake = if tailMode gameState && newHead == food gameState
                      then tail (snake gameState) ++ [newHead]
                      else if newHead == food gameState
                      then newHead : snake gameState
                      else newHead : init (snake gameState)
      if collision newHead (newSnake ++ walls gameState)
        then return gameState { alive = False, screen = GameOver }
        else do
          newFood <- if newHead == food gameState then randomFoodPosition else return (food gameState)
          return gameState
            { snake = newSnake
            , food = newFood
            , score = if newHead == food gameState then score gameState + 1 else score gameState
            , hiScore = max (score gameState + 1) (hiScore gameState)
            }

updateLeaderboard :: Int -> [Int] -> [Int]
updateLeaderboard score lb = take 10 (insert score lb)

move :: Direction -> Position -> Position
move U (x, y) = (x, y + 1)
move D (x, y) = (x, y - 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

collision :: Position -> [Position] -> Bool
collision pos body = pos `elem` body || outOfBounds pos
  where
    outOfBounds (x, y) = abs x > w || abs y > h
    w = windowWidth `div` (2 * blockSize)
    h = windowHeight `div` (2 * blockSize)

-- Event Handling
handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState =
  return $ if dir gameState /= D then gameState { dir = U } else gameState
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState =
  return $ if dir gameState /= U then gameState { dir = D } else gameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState =
  return $ if dir gameState /= R then gameState { dir = L } else gameState
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState =
  return $ if dir gameState /= L then gameState { dir = R } else gameState
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gameState
  | screen gameState == Start = return gameState { screen = Game }
  | screen gameState == GameOver = initialState
handleKeys (EventKey (Char 'l') Down _ _) gameState
  | screen gameState == GameOver = return gameState { screen = Leaderboard }
  | screen gameState == Start = return gameState { walls = nextWalls (walls gameState) }
  where
    nextWalls [] = [(x, 5) | x <- [-10..10]]
    nextWalls _  = []
handleKeys (EventKey (Char 't') Down _ _) gameState
  | screen gameState == Start = return gameState { tailMode = not (tailMode gameState) }
handleKeys (EventKey (Char 'd') Down _ _) gameState
  | screen gameState == Start = return gameState { duoMode = not (duoMode gameState) }
handleKeys _ gameState = return gameState

-- Main function
main :: IO ()
main = do
  state <- initialState
  playIO
    (InWindow "Snake Game" (windowWidth, windowHeight) (100, 100))
    azure
    5
    state
    (return . render)
    handleKeys
    update
