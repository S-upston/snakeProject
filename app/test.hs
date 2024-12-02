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
  { snake :: [Position]  -- List of positions representing the snake
  , dir   :: Direction   -- Current direction
  , food  :: Position    -- Food position
  , alive :: Bool        -- Is the game running
  }

-- Convert game coordinates to Gloss coordinates
toGlossCoord :: Position -> (Float, Float)
toGlossCoord (x, y) = (fromIntegral x * fromIntegral blockSize, fromIntegral y * fromIntegral blockSize)

-- Render the game state
render :: GameState -> Picture
render (GameState snake _ food alive) = pictures $ snakePic ++ [foodPic, border]
  where
    snakePic = map (color green . translateBlock) snake
    foodPic  = color red $ translateBlock food
    border   = color white $ rectangleWire (fromIntegral windowWidth) (fromIntegral windowHeight)
    translateBlock pos = translate x y $ rectangleSolid size size
      where
        (x, y) = toGlossCoord pos
        size = fromIntegral blockSize

-- Initial game state
initialState :: IO GameState
initialState = do
  foodPos <- randomFoodPosition
  return $ GameState [(0, 0)] R foodPos True

randomFoodPosition :: IO Position
randomFoodPosition = do
  x <- randomRIO (-w, w)
  y <- randomRIO (-h, h)
  return (x, y)
  where
    w = windowWidth `div` (2 * blockSize) - 1
    h = windowHeight `div` (2 * blockSize) - 1


update :: Float -> GameState -> IO GameState
update _ gameState
  | not (alive gameState) = return gameState
  | otherwise = do
      let newHead = move (dir gameState) (head $ snake gameState)
          newSnake = if newHead == food gameState
                      then newHead : snake gameState
                      else newHead : init (snake gameState)
      if collision newHead newSnake
        then return gameState { alive = False }
        else if newHead == food gameState
          then do
            newFood <- randomFoodPosition
            return gameState { snake = newSnake, food = newFood }
          else return gameState { snake = newSnake }


move :: Direction -> Position -> Position
move U (x, y) = (x, y + 1)
move D (x, y) = (x, y - 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)


collision :: Position -> [Position] -> Bool
collision pos body = pos `elem` tail body || outOfBounds pos
  where
    outOfBounds (x, y) = abs x > w || abs y > h
    w = windowWidth `div` (2 * blockSize)
    h = windowHeight `div` (2 * blockSize)


handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) gameState =
  return $ if dir gameState /= D then gameState { dir = U } else gameState
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) gameState =
  return $ if dir gameState /= U then gameState { dir = D } else gameState
handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) gameState =
  return $ if dir gameState /= R then gameState { dir = L } else gameState
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) gameState =
  return $ if dir gameState /= L then gameState { dir = R } else gameState
handleKeys (EventKey (Char 'r') Down _ _) _ = initialState
handleKeys (EventKey (Char 'p') Down _ _) gameState = 
  return $ gameState { alive = not (alive gameState) } 
handleKeys _ gameState = return gameState



main :: IO ()
main = do
  state <- initialState
  playIO
    (InWindow "Snake Game" (windowWidth, windowHeight) (100, 100)) -- Window configuration
    black      
    5
    state   
    (return . render) 
    handleKeys 
    update     

