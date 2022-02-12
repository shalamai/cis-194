{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate ( i )
import System.IO
import Data.List (intercalate)

-- code world (terminal version)

type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun

blank :: Picture
blank = id

pictureOf :: Char -> Picture
pictureOf c _ 0 0 = c
pictureOf _ d x y = d x y

(&) :: Picture -> Picture -> Picture
(&) = (.)

translated :: Integer -> Integer -> Picture -> Picture
translated r' c' p d r c = p (\a b -> d (a + r') (b + c')) (r - r') (c - c')

draw :: Picture -> String
draw p =  drawRaws [-11..11]
  where
    drawRaws = intercalate "\n" . reverse . map (\r -> drawRaw r [-11..11])
    drawRaw r = concatMap (drawCol . p (\ _ _ -> ' ') r)
    drawCol '*' = "\ESC[100m  \ESC[0m"
    drawCol '.' = "\ESC[107m  \ESC[0m"
    drawCol '_' = "\ESC[107m⭕\ESC[0m"
    drawCol '0' = "\ESC[107m📦\ESC[0m"
    drawCol '>' = "\ESC[107m😏\ESC[0m"
    drawCol '<' = "\ESC[107m🚕\ESC[0m"
    drawCol 'V' = "\ESC[107m😞\ESC[0m"
    drawCol 'A' = "\ESC[107m😱\ESC[0m"
    drawCol c = [c]


-- is graph closed

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go [] [initial]
  where
    go _ [] = True
    go visited (a:as)
      | a `elem` visited = go visited as
      | not (isOk a) = False
      | otherwise = go (a:visited) (adjacent a ++ as)

-- area

data Direction = U | D | R | L deriving Eq
data Coord = C Integer Integer deriving Eq

coords :: [Coord]
coords = do
  r <- [-11..11]
  C r <$> [-11..11]

allDirections :: [Direction]
allDirections = [U, D, R, L]

calcCoord :: Direction -> Coord -> Coord
calcCoord U (C x y) = C x (y + 1)
calcCoord D (C x y) = C x (y - 1)
calcCoord R (C x y) = C (x + 1) y
calcCoord L (C x y) = C (x - 1) y

-- maze

data Tile = Wall | Ground | Storage | Box | None deriving Eq

pictureOfMaze :: Maze -> Picture
pictureOfMaze m = foldl (&) blank (map (drawTileAt m) coords)

pictureOfBoxes :: [Coord] -> Picture
pictureOfBoxes cs = foldl (&) blank (map (\c -> atCoord c (drawTile Box)) cs)

drawTileAt :: Maze -> Coord -> Picture
drawTileAt m c = atCoord c (drawTile (noBoxMaze m c))

drawTile :: Tile -> Picture
drawTile tile = case tile of
  Wall -> pictureOf '*'
  Ground -> pictureOf '.'
  Storage -> pictureOf '_'
  Box -> pictureOf '0'
  None -> blank

noBoxMaze :: Maze -> Maze
noBoxMaze m c = case m c of
  Box -> Ground
  t -> t

mazeWithBoxes :: Maze -> [Coord] -> Maze
mazeWithBoxes m boxes = go
  where
    go c | c `elem` boxes = Box
    go c = noBoxMaze m c

isOnStorage :: Maze -> Coord -> Bool
isOnStorage m c = m c == Storage

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated y x

isClosed :: MazeWithStart -> Bool
isClosed (MazeWithStart start m) =
  (m start == Ground || m start == Storage) && isGraphClosed start adjacent isOk
  where
    adjacent c = filter isReachable (allAdjacent c)
    allAdjacent c = map (`calcCoord` c) allDirections

    isOk = \c -> m c /= None
    isReachable = \c -> m c /= Wall



-- player

player :: Direction -> Picture
player U = pictureOf 'A'
player D = pictureOf 'V'
player R = pictureOf '>'
player L = pictureOf '<'

-- resettable activity

reseteble :: Activity world -> Activity world
reseteble (Activity w0 handle draw) = Activity w0 handle' draw
  where
    handle' "r" w = w0
    handle' e w = handle e w

-- activity with start screen
data StartState world = StartScreen | Running world deriving Eq

withStartScreen :: Activity world -> Activity (StartState world)
withStartScreen (Activity w0 handle draw) = Activity w0' handle' draw'
  where
    w0' = StartScreen

    handle' " " StartScreen = handle' " " (Running w0)
    handle' _ StartScreen = StartScreen
    handle' e (Running w) = Running (handle e w)

    draw' StartScreen = [i|
              !! sokoban !!

      control scheme:
          wasd/arrows    - move
          r              - restart
          u              - undo
          q              - quit
        


      ready??? press "space" to start 
    |]
    draw' (Running w) = draw w


-- activity with undo
data WithUndo world = WithUndo world [world]

withUndo :: Eq w => Activity w -> Activity (WithUndo w)
withUndo (Activity w0 handle draw) = Activity w0' handle' draw'
  where
    w0' = WithUndo w0 []

    handle' "u" s@(WithUndo _ []) = s
    handle' "u" (WithUndo _ (w:stack)) = WithUndo w stack
    handle' e s@(WithUndo w stack) = if w == w' then s else WithUndo w' (w:stack)
      where
        w' = handle e w

    draw' (WithUndo w _) = draw w


-- activity
data Activity world = Activity world (String -> world -> world) (world -> String)

runActivity :: Activity world -> IO()
runActivity (Activity w0 handle draw) =
  do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    go w0 ""
    where
      go w prevInput = do
        putStr "\ESCc"
        putStr $ draw w
        input <- getAllInput
        let allInput = prevInput ++ input
        let (c:restInput) = case allInput of
                      '\ESC':'[':'A':rest -> 'w':rest
                      '\ESC':'[':'B':rest -> 's':rest
                      '\ESC':'[':'C':rest -> 'd':rest
                      '\ESC':'[':'D':rest -> 'a':rest
                      otherwise           -> input

        let w' = handle [c] w
        if c == 'q' then putStr "bye !!" else go w' restInput


getAllInput :: IO String
getAllInput = do
  c <- getChar
  ready <- hReady stdin
  rest <- if ready then getAllInput else return []
  return (c : rest)

-- sokoban
data AppState = S Direction Coord [Coord] Integer deriving Eq

loadLevel :: Integer -> AppState
loadLevel level = S U start (initialBoxes m) level
  where
    MazeWithStart start m = mazes !! fromIntegral level

initialBoxes :: Maze -> [Coord]
initialBoxes m = filter isBox coords
  where
    isBox c = case m c of
      Box -> True
      _ -> False

handleEvent :: String -> AppState -> AppState
handleEvent " " s@(S dir player boxes level) | isPeremoha s = loadLevel (level + 1)
handleEvent _ s | isPeremoha s = s
handleEvent k s = case k of
  "w" -> moveIfPossible U s
  "s" -> moveIfPossible D s
  "d" -> moveIfPossible R s
  "a" -> moveIfPossible L s
  _ -> s

moveIfPossible :: Direction -> AppState -> AppState
moveIfPossible dir (S _ c boxes level) = state'
  where
    state' = if isPlayerMovable destination then S dir destination boxes' level else S dir c boxes level
    destination = calcCoord dir c
    boxes' = map (moveFromTo destination (calcCoord dir destination)) boxes
    moveFromTo from to c = if from == c then to else c
    maze = nthMaze level

    isPlayerMovable to = case mazeWithBoxes maze boxes to of
      Ground -> True
      Storage -> True
      Box -> isBoxMovable (calcCoord dir to)
      _ -> False

    isBoxMovable to = case mazeWithBoxes maze boxes to of
      Ground -> True
      Storage -> True
      _ -> False


drawState :: AppState -> String
drawState s@(S _ _ _ level) | isPeremoha s = if isLastLevel then sokobanDoneMsg else levelDoneMsg
  where
    levelDoneMsg = [i|level #{level + 1} of #{totalLevels} completed! (press "space" to play next)|]
    sokobanDoneMsg = "whoohoo! you've completed all levels!"
    totalLevels = length mazes
    isLastLevel = level == fromIntegral totalLevels

drawState (S dir c boxes level) = draw p
  where
    p = atCoord c (player dir) & pictureOfBoxes boxes & pictureOfMaze (nthMaze level)

sokoban :: Activity AppState
sokoban = Activity (loadLevel 0) handleEvent drawState

isPeremoha :: AppState -> Bool
isPeremoha (S _ _ boxes level) = all (isOnStorage (nthMaze level)) boxes

-- main

main :: IO ()
main = runActivity (reseteble (withStartScreen (withUndo sokoban)))

-- mazes 
type Maze = Coord -> Tile
data MazeWithStart = MazeWithStart Coord Maze

nthMaze :: Integer -> Maze
nthMaze n = m
  where
    MazeWithStart _ m = mazes !! fromIntegral n

mazes :: [MazeWithStart]
mazes = [
  MazeWithStart (C 1 1)       maze9,
  MazeWithStart (C 0 0)       maze8,
  MazeWithStart (C (-3) 3)    maze7,
  MazeWithStart (C (-2) 4)    maze6,
  MazeWithStart (C 0 1)       maze5,
  MazeWithStart (C 1 (-3))    maze4,
  MazeWithStart (C (-4) 3)    maze3,
  MazeWithStart (C 0 1)       maze1
  ]

extraMazes :: [MazeWithStart]
extraMazes = [
  MazeWithStart (C 1 (-3))    maze4' ,
  MazeWithStart (C 1 (-3))    maze4'',
  MazeWithStart (C 1 1)       maze9'
  ] ++ mazes

maze1 :: Maze
maze1 (C x y)
  | abs x > 4  || abs y > 4  = None
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze3 :: Maze
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5)   0 ) = Wall
maze3 (C (-5)   1 ) = Wall
maze3 (C (-5)   2 ) = Wall
maze3 (C (-5)   3 ) = Wall
maze3 (C (-5)   4 ) = Wall

maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4)   0 ) = Ground
maze3 (C (-4)   1 ) = Ground
maze3 (C (-4)   2 ) = Ground
maze3 (C (-4)   3 ) = Ground
maze3 (C (-4)   4 ) = Wall

maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3)   0 ) = Wall
maze3 (C (-3)   1 ) = Ground
maze3 (C (-3)   2 ) = Wall
maze3 (C (-3)   3 ) = Ground
maze3 (C (-3)   4 ) = Wall
maze3 (C (-3)   5 ) = Wall

maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2)   0 ) = Wall
maze3 (C (-2)   1 ) = Ground
maze3 (C (-2)   2 ) = Box
maze3 (C (-2)   3 ) = Box
maze3 (C (-2)   4 ) = Ground
maze3 (C (-2)   5 ) = Wall

maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1)   0 ) = Wall
maze3 (C (-1)   1 ) = Ground
maze3 (C (-1)   2 ) = Ground
maze3 (C (-1)   3 ) = Box
maze3 (C (-1)   4 ) = Ground
maze3 (C (-1)   5 ) = Wall
maze3 (C (-1)   6 ) = Wall

maze3 (C   0  (-6)) = Wall
maze3 (C   0  (-5)) = Ground
maze3 (C   0  (-4)) = Ground
maze3 (C   0  (-3)) = Ground
maze3 (C   0  (-2)) = Ground
maze3 (C   0  (-1)) = Ground
maze3 (C   0    0 ) = Wall
maze3 (C   0    1 ) = Wall
maze3 (C   0    2 ) = Wall
maze3 (C   0    3 ) = Wall
maze3 (C   0    4 ) = Ground
maze3 (C   0    5 ) = Ground
maze3 (C   0    6 ) = Wall

maze3 (C   1  (-6)) = Wall
maze3 (C   1  (-5)) = Ground
maze3 (C   1  (-4)) = Ground
maze3 (C   1  (-3)) = Ground
maze3 (C   1  (-2)) = Ground
maze3 (C   1  (-1)) = Ground
maze3 (C   1    0 ) = Wall
maze3 (C   1    1 ) = Storage
maze3 (C   1    2 ) = Storage
maze3 (C   1    3 ) = Storage
maze3 (C   1    4 ) = Ground
maze3 (C   1    5 ) = Ground
maze3 (C   1    6 ) = Wall

maze3 (C   2  (-6)) = Wall
maze3 (C   2  (-5)) = Wall
maze3 (C   2  (-4)) = Ground
maze3 (C   2  (-3)) = Ground
maze3 (C   2  (-2)) = Ground
maze3 (C   2  (-1)) = Ground
maze3 (C   2    0 ) = Wall
maze3 (C   2    1 ) = Wall
maze3 (C   2    2 ) = Wall
maze3 (C   2    3 ) = Wall
maze3 (C   2    4 ) = Wall
maze3 (C   2    5 ) = Wall
maze3 (C   2    6 ) = Wall

maze3 (C   3  (-5)) = Wall
maze3 (C   3  (-4)) = Ground
maze3 (C   3  (-3)) = Ground
maze3 (C   3  (-2)) = Storage
maze3 (C   3  (-1)) = Ground
maze3 (C   3    0 ) = Wall

maze3 (C   4  (-5)) = Wall
maze3 (C   4  (-4)) = Wall
maze3 (C   4  (-3)) = Wall
maze3 (C   4  (-2)) = Wall
maze3 (C   4  (-1)) = Wall
maze3 (C   4    0 ) = Wall

maze3 _ = None

maze4 :: Maze
maze4 (C x y)
  | abs x > 4  || abs y > 4      = None
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground

maze5 :: Maze
maze5 (C x y)
  | abs x >  4 || abs y >  4           = None
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

maze6 :: Maze
maze6 (C x y)
  | abs x > 3  || abs y > 5                 = None
  | abs x == 3 || abs y == 5 && abs x < 4 = Wall
  | x == 0 && abs y < 4                     = Storage
  | x == -1 && (y == 0 || abs y == 2)       = Box
  | x == 1 && (abs y == 1 || abs y == 3)    = Box
  | x == (-2) &&  y == 1                    = Wall
  | otherwise                               = Ground

maze7 :: Maze
maze7 (C x y)
  | abs x > 4  || abs y > 4   = None
  | abs x == 4 || abs y == 4  = Wall
  | x /= 2  && y == 2   = Wall
  | x /= (-2)  && y == -1 = Wall
  | x ==  3 && y == -3        = Storage
  | x == 2 && y == 2          = Box
  | otherwise                 = Ground

maze8 :: Maze
maze8 (C x y)
  | abs x > 10 || abs y > 10    = None
  | x == 0 && y == 0            = Ground
  | abs x == 9 && abs y == 9    = Wall
  | abs x == 10 || abs y == 10  = Wall
  | x == y                      = Storage
  | abs x == abs y              = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0    = Storage
  | otherwise                   = Ground

maze9 :: Maze
maze9 (C x y)
  | abs x > 4  || abs y > 4                  = None
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground

maze4'' :: Maze
maze4'' (C 1 (-3)) = Box
maze4'' c = maze4 c

maze4' :: Maze
maze4' (C 0 1) = None
maze4' c = maze4 c

maze9' :: Maze
maze9' (C 3 0) = Box
maze9' (C 4 0) = Box
maze9'  c      = maze9 c
