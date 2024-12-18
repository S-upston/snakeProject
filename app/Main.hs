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
  , leaderboard :: [Int]
  , level      :: Int 
  }
data Screen = Start | Game | GameOver | Leaderboard deriving (Eq)

--Handles which screen should be rendered
render :: GameState -> Picture
render gameState = case screen gameState of
  Start        -> renderStartScreen gameState
  Game         -> renderGameScreen gameState
  GameOver     -> renderGameOverScreen gameState
  Leaderboard  -> renderLeaderboardScreen (leaderboard gameState)

--Renders the start scren
renderStartScreen :: GameState -> Picture
renderStartScreen gameState = pictures
  [ translate (-130) 100 (scale 0.2 0.2 (color white (text "Press ENTER to Start")))
  , translate (-170) 50 (scale 0.2 0.2 (color white (text "Use ARROW KEYS to move")))
  , translate (-175) (-50) (scale 0.2 0.2 (color white (text "Press T to toggle Tail Mode")))
  , translate (-195) (-100) (scale 0.2 0.2 (color white (text "Press 0,1,2,3,4,5 to select level")))
  , translate (-160) (-150) (scale 0.2 0.2 (color white (text ("Level: " ++ show (level gameState) ++ " Tail Mode: " ++ show(tailMode gameState)))))
  ]

--Renders the game screen
renderGameScreen :: GameState -> Picture
renderGameScreen (GameState snake _ food _ score hiScore _ walls _ _ _) = pictures (
  [ translateBlock pos (color green (rectangleSolid size size)) | pos <- snake ] ++
  [ translateBlock pos (color white (rectangleSolid size size)) | pos <- walls ] ++
  [ translateBlock food (color red (rectangleSolid size size)) ] ++
  [ translate (-fromIntegral windowWidth / 2 + 10) (fromIntegral windowHeight / 2 - 30)
      (scale 0.1 0.1 (color white (text ( "Score: " ++ show score ++ " Hi-Score: " ++ show hiScore)))) ] ++
  [color white (rectangleWire (fromIntegral windowWidth) (fromIntegral windowHeight))]  ) 
  where
    translateBlock (x, y) block = translate (fromIntegral x * fromIntegral blockSize) (fromIntegral y * fromIntegral blockSize) block
    size = fromIntegral blockSize

--Renders the game over screen
renderGameOverScreen :: GameState -> Picture
renderGameOverScreen gameState = pictures
  [ translate (-110) 50 (scale 0.3 0.3 (color white (text "Game Over")))
  , translate (-150) (-50) (scale 0.2 0.2 (color white (text $ "Score: " ++ show (score gameState) ++ "   Hi-Score: " ++ show(hiScore gameState))))
  , translate (-150) (-100) (scale 0.2 0.2 (color white (text "Press ENTER to Restart")))
  , translate (-150) (-150) (scale 0.2 0.2 (color white (text "Press L for Leaderboard")))
  ]
  
--Renders the leaderboard screen
renderLeaderboardScreen :: [Int] -> Picture
renderLeaderboardScreen scores = pictures (
  [ translate (-130) 100 (scale 0.3 0.3 (color white (text "Leaderboard"))) ] ++
   zipWith (\y (place, score) -> translate (-100) y (scale 0.2 0.2 (color white (text $ show place ++ ".       " ++ show score)))) [50, 20..] (zip [1..] ((take 10 (reverse scores)))))

--Initial state for the game
initialState :: IO GameState
initialState = do
  foodPos <- randomPosition
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
    , leaderboard = []
    , level = 0  
    }
--Generates a random food position until the position isn't in the wall    
randomFoodPosition :: GameState -> IO Position
randomFoodPosition gameState = do
  newFoodPos <- randomPosition
  if newFoodPos `elem` (levelWalls (level gameState))
    then randomFoodPosition gameState
    else return newFoodPos

--Generates a random position
randomPosition :: IO Position
randomPosition = do
  x <- randomRIO (-w, w)
  y <- randomRIO (-h, h)
  return (x, y)
  where
    w = windowWidth `div` (2 * blockSize) - 1
    h = windowHeight `div` (2 * blockSize) - 1


--Defines the wall locations for each level    
levelWalls :: Int -> [Position]
levelWalls 0 = []
levelWalls 1 = [(-5,-5),(-4,-5),(-5,-4),(5,5),(4,5),(5,4),(-5,5),(-4,5),(-5,4),(5,-5),(4,-5),(5,-4)] -- Simple walls, cross shape
levelWalls 2 = [(x,y) | x <- [-5,5], y <- [0..5]] ++
               [(x,y) | x <- [-3..3], y <- [-6]] ++
               [(-4,-5),(-5,-4),(4,-5),(5,-4)]
levelWalls 3 = [(x,y) | x <- [5], y <- [-5..5]] ++
               [(x,y) | x <- [3..7], y <-[5,-5], x /= 5] ++
               [(x,y) | x <- [-5], y <- [-5..5]] ++
               [(x,y) | x <- [-7.. -3], y <-[5,-5], x /= -5] 
levelWalls 4 = [(x,y) | x <- [-5], y <- [-5..5], y /= 0] ++
               [(x,y) | x <- [5], y <- [-5..5], y /= 0]  ++
               [(x,y) | y <- [-5], x <- [-4..4], x /= 0] ++
               [(x,y) | y <- [5], x <- [-4..4], x /= 0] 
levelWalls 5 = [(x,y) | x <- [-5], y <- [-5..5], y /= 0] ++
               [(x,y) | x <- [5], y <- [-5..5], y /= 0]  ++
               [(x,y) | y <- [-5], x <- [-4..4], x /= 0] ++
               [(x,y) | y <- [5], x <- [-4..4], x /= 0] ++
               [(x,y) | x <- [-8], y <- [-8..8], y /= 0] ++
               [(x,y) | x <- [8], y <- [-8..8], y /= 0]  ++
               [(x,y) | y <- [-8], x <- [-7..7], x /= 0] ++
               [(x,y) | y <- [8], x <- [-7..7], x /= 0] ++
               [(2,2),(2,1),(1,2),(-2,2),(-2,1),(-1,2),(2,-2),(2,-1),(1,-2),(-2,-2),(-2,-1),(-1,-2)]


--Updates the game when needed
update :: Float -> GameState -> IO GameState
update _ gameState = case screen gameState of
  Start        -> return gameState
  Game         -> updateGame gameState
  GameOver     -> return gameState
  Leaderboard  -> return gameState
--Updates the game
updateGame :: GameState -> IO GameState
updateGame gameState
  | not (alive gameState) = return gameState { screen = GameOver }
  | otherwise = do
      --Makes a new head and handles the necessary logic if tail mode is enabled 
      let tailHead = move (dir gameState) (head (snake gameState))
          newDir
            | tailMode gameState && tailHead == food gameState = tailDirection (snake gameState)
            | otherwise = dir gameState
          newHead 
            | tailMode gameState && tailHead == food gameState = last (snake gameState)
            | otherwise = move (dir gameState) (head (snake gameState))
          newSnake
            | tailMode gameState && tailHead == food gameState = 
                newHead : reverse(tailHead:(init(snake gameState)))
            | newHead == food gameState = 
                newHead : snake gameState
            | otherwise = 
                newHead : init (snake gameState)
      -- Checks for collision after adding the new head
      if collision newHead (newSnake ++ walls gameState)
        then return gameState { alive = False, screen = GameOver, leaderboard = updateLeaderboard (score gameState) (leaderboard gameState) }
        else do
          newFood <- if tailHead == food gameState then randomFoodPosition gameState else return (food gameState)
          return gameState
            { snake = newSnake
            , food = newFood
            , score = if tailHead == food gameState then score gameState + 1 else score gameState
            , hiScore = max (score gameState) (hiScore gameState)
            , dir = newDir
            }
            
--Handles getting the new direction after food is eaten in tail mode
tailDirection :: [Position] -> Direction
tailDirection snake 
  | (0,1) == newDir = D
  | (0,-1) == newDir = U
  | (1,0) == newDir = L
  | (-1,0) == newDir = R
  where
    newDir = getTailComponent (drop ((length snake) - 2) snake)
 --Helper function for tailDirection       
getTailComponent :: [Position] -> Position
getTailComponent [(x1,y1),(x2,y2)] = (x1-x2,y1-y2)

--Updates leaderboard with new score
updateLeaderboard :: Int -> [Int] -> [Int]
updateLeaderboard score lb = take 10 (insert score lb)

--Returns the corresponding postion translation for each direction
move :: Direction -> Position -> Position
move U (x, y) = (x, y + 1)
move D (x, y) = (x, y - 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

--Checks for collision 
collision :: Position -> [Position] -> Bool
collision pos body = pos `elem` tail body || outOfBounds pos
  where
    outOfBounds (x, y) = abs x > w || abs y > h
    w = windowWidth `div` (2 * blockSize)
    h = windowHeight `div` (2 * blockSize)


    

--Handles keyboard input
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
  | screen gameState == GameOver = do
            foodPos <- randomPosition
            return GameState
              { snake = [(0, 0), (-1, 0), (-2, 0), (-3, 0)]  
              , dir = R
              , food = foodPos
              , alive = True
              , score = 0
              , hiScore = hiScore gameState
              , screen = Start
              , walls = []
              , tailMode = False
              , leaderboard = leaderboard gameState
              , level = 0  
              }
  | screen gameState == Leaderboard = return gameState { screen = GameOver } 
handleKeys (EventKey (Char 'l') Down _ _) gameState
  | screen gameState == GameOver = return gameState { screen = Leaderboard }
  | screen gameState == Start = return gameState { walls = nextWalls (walls gameState) }
  where
    nextWalls [] = levelWalls (level gameState)
    nextWalls _  = []
handleKeys (EventKey (Char '0') Down _ _) gameState = return gameState { level = 0, walls = levelWalls 0 }
handleKeys (EventKey (Char '1') Down _ _) gameState = return gameState { level = 1, walls = levelWalls 1 }
handleKeys (EventKey (Char '2') Down _ _) gameState = return gameState { level = 2, walls = levelWalls 2 }
handleKeys (EventKey (Char '3') Down _ _) gameState = return gameState { level = 3, walls = levelWalls 3 }
handleKeys (EventKey (Char '4') Down _ _) gameState = return gameState { level = 4, walls = levelWalls 4 }
handleKeys (EventKey (Char '5') Down _ _) gameState = return gameState { level = 5, walls = levelWalls 5 }
handleKeys (EventKey (Char 't') Down _ _) gameState
  | screen gameState == Start = return gameState { tailMode = not (tailMode gameState) }
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
