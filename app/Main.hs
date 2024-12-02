import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)

-- Game configuration
windowWidth, windowHeight, blockSize :: Int
windowWidth = 400
windowHeight = 400
blockSize = 20

data Direction = U | D | L | R deriving (Eq)
type Position = (Int, Int)
data GameState = GameState
  { snake  :: [Position]  -- List of positions representing the snake
  , dir    :: Direction   -- Current direction
  , food   :: Position    -- Food position
  , alive  :: Bool        -- Is the game running
  , score  :: Int         -- Player's score
  , screen :: Screen      -- Current screen (Start, Game, GameOver)
  }
data Screen = Start | Game | GameOver deriving (Eq)

-- Convert game coordinates to Gloss coordinates
toGlossCoord :: Position -> (Float, Float)
toGlossCoord (x, y) = (fromIntegral x * fromIntegral blockSize, fromIntegral y * fromIntegral blockSize)

-- Render the game state
render :: GameState -> Picture
render gameState = case screen gameState of
  Start    -> renderStartScreen
  Game     -> renderGameScreen gameState
  GameOver -> renderGameOverScreen gameState

-- Render start screen
renderStartScreen :: Picture
renderStartScreen = translate (-130) 50 (scale 0.2 0.2 (color white (text "Press SPACE to Start"))) <>
                    translate (-120) (-50) (scale 0.2 0.2 (color white (text "Use arrow keys to"))) <>
                    translate (-100) (-100) (scale 0.2 0.2 (color white(text"move the snake")))

-- Render game screen
renderGameScreen :: GameState -> Picture
renderGameScreen (GameState snake _ food _ score _) = pictures $ snakePic ++ [foodPic, border, scoreDisplay]
  where
    snakePic = map (color green . translateBlock) snake
    foodPic  = color red $ translateBlock food
    border   = color white $ rectangleWire (fromIntegral windowWidth) (fromIntegral windowHeight)
    scoreDisplay = translate (-fromIntegral windowWidth / 2 + 10) (fromIntegral windowHeight / 2 - 30)
                     (scale 0.2 0.2 (color white (text $ "Score: " ++ show score)))
    translateBlock pos = translate x y $ rectangleSolid size size
      where
        (x, y) = toGlossCoord pos
        size = fromIntegral blockSize

-- Render game over screen
renderGameOverScreen :: GameState -> Picture
renderGameOverScreen gameState = translate (-110) 50 (scale 0.3 0.3 (color white (text "Game Over"))) <>
                                 translate (-100) (-50) (scale 0.2 0.2 (color white (text $ "Final Score: " ++ show (score gameState)))) <>
                                 translate (-150) (-150) (scale 0.2 0.2 (color white (text "Press SPACE to Restart")))

-- Initial game state
initialState :: IO GameState
initialState = do
  foodPos <- randomFoodPosition
  return $ GameState
    { snake = [(0, 0), (-1, 0), (-2, 0), (-3, 0)] -- Start with 4 body parts
    , dir = R
    , food = foodPos
    , alive = True
    , score = 0
    , screen = Start
    }

-- Random food position
randomFoodPosition :: IO Position
randomFoodPosition = do
  x <- randomRIO (-w, w)
  y <- randomRIO (-h, h)
  return (x, y)
  where
    w = windowWidth `div` (2 * blockSize) - 1
    h = windowHeight `div` (2 * blockSize) - 1

-- Update the game state
update :: Float -> GameState -> IO GameState
update _ gameState = case screen gameState of
  Start    -> return gameState
  Game     -> updateGame gameState
  GameOver -> return gameState

updateGame :: GameState -> IO GameState
updateGame gameState
  | not (alive gameState) = return gameState { screen = GameOver }
  | otherwise = do
      let newHead = move (dir gameState) (head $ snake gameState)
          newSnake = if newHead == food gameState
                      then newHead : snake gameState
                      else newHead : init (snake gameState)
      if collision newHead newSnake
        then return gameState { alive = False, screen = GameOver }
        else if newHead == food gameState
          then do
            newFood <- randomFoodPosition
            return gameState { snake = newSnake, food = newFood, score = score gameState + 1 }
          else return gameState { snake = newSnake }

-- Move the snake
move :: Direction -> Position -> Position
move U (x, y) = (x, y + 1)
move D (x, y) = (x, y - 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

-- Check for collisions
collision :: Position -> [Position] -> Bool
collision pos body = pos `elem` tail body || outOfBounds pos
  where
    outOfBounds (x, y) = abs x > w || abs y > h
    w = windowWidth `div` (2 * blockSize)
    h = windowHeight `div` (2 * blockSize)

-- Handle key events
handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState =
  return $ if dir gameState /= D then gameState { dir = U } else gameState
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState =
  return $ if dir gameState /= U then gameState { dir = D } else gameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState =
  return $ if dir gameState /= R then gameState { dir = L } else gameState
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState =
  return $ if dir gameState /= L then gameState { dir = R } else gameState
handleKeys (EventKey (Char 'p') Down _ _) gameState = case screen gameState of
  Start    -> return gameState { screen = Game }
  GameOver -> initialState
  _        -> return gameState
handleKeys _ gameState = return gameState

-- Main function
main :: IO ()
main = do
  state <- initialState
  playIO
    (InWindow "Snake Game" (windowWidth, windowHeight) (100, 100)) -- Window configuration
    aquamarine       -- Background color
    5           -- Frames per second
    state       -- Initial state
    (return . render) -- Render the game
    handleKeys  -- Handle key inputs
    update      -- Update the game state

