{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate ( i )

import CodeWorld

-- list

data List a = Empty | Elem a (List a) deriving (Eq, Show)

infixr `Elem`

combine :: b -> (a -> b -> b) -> List a -> b
combine z _ Empty = z
combine z f (Elem a as) = f a (combine z f as)

mapList :: (a -> b) -> List a -> List b
mapList f = combine Empty (Elem . f)

append :: List a -> List a -> List a
append a b = combine b Elem a

filterList :: (a -> Bool) -> List a -> List a
filterList p = combine Empty (\a acc -> if p a then Elem a acc else acc)

exists :: (a -> Bool) -> List a -> Bool
exists p as = combine False (||) (mapList p as)

forall ::  (a -> Bool) -> List a -> Bool
forall p as = combine True (&&) (mapList p as)

elemList :: Eq a => a -> List a -> Bool
elemList a = exists (a ==)

listLength :: List a -> Integer
listLength = combine 0 (\_ acc -> acc + 1)

nth :: List a -> Integer -> a
nth Empty _ = error "list too short"
nth (Elem a _) 1 = a
nth (Elem _ as) n = nth as (n - 1)

-- fold21

fold21 :: a -> (a -> a -> a) -> (Integer -> a) -> a
fold21 z combine f = go (-10)
  where
    go n
      | n > 10 = z
      | otherwise = combine (f n) (go (n + 1))


-- is graph closed

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go Empty (Elem initial Empty)
  where
    go _ Empty = True
    go visited (Elem a as)
      | elemList a visited = go visited as
      | not (isOk a) = False
      | otherwise = go (Elem a visited) (append as (adjacent a))

-- area

data Direction = U | D | R | L deriving Eq
data Coord = C Integer Integer deriving Eq

coords :: List Coord
coords = fold21 Empty append (\r -> fold21 Empty append (\c -> Elem (C r c) Empty))

allDirections :: List Direction
allDirections = Elem U (Elem D (Elem R (Elem L Empty)))

calcCoord :: Direction -> Coord -> Coord
calcCoord U (C x y) = C x (y + 1)
calcCoord D (C x y) = C x (y - 1)
calcCoord R (C x y) = C (x + 1) y
calcCoord L (C x y) = C (x - 1) y

-- maze

data Tile = Wall | Ground | Storage | Box | None deriving Eq

pictureOfMaze :: Maze -> Picture
pictureOfMaze m = combine blank (&) (mapList (drawTileAt m) coords)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine blank (&) (mapList (\c -> atCoord c (drawTile Box)) cs)

drawTileAt :: Maze -> Coord -> Picture
drawTileAt m c = atCoord c (drawTile (noBoxMaze m c))

drawTile :: Tile -> Picture
drawTile tile = case tile of
  Wall -> colored gray (solidRectangle 1 1)
  Ground -> thickRectangle 0.005 1 1
  Storage -> colored red (solidCircle 0.1) & thickRectangle 0.005 1 1
  Box -> colored white (rectangle 1 1) & colored (lighter 0.2 black) (solidRectangle 1 1)
  None -> blank

noBoxMaze :: Maze -> Maze
noBoxMaze m c = case m c of
  Box -> Ground
  t -> t

mazeWithBoxes :: Maze -> List Coord -> Maze
mazeWithBoxes m boxes = go
  where
    go c | exists (== c) boxes = Box
    go c = noBoxMaze m c

isOnStorage :: Maze -> Coord -> Bool
isOnStorage m c = m c == Storage

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated (fromIntegral x) (fromIntegral y)

isClosed :: MazeWithStart -> Bool
isClosed (MazeWithStart start m) =
  (m start == Ground || m start == Storage) && isGraphClosed start adjacent isOk
  where
    adjacent c = filterList isReachable (allAdjacent c)
    allAdjacent c = mapList (`calcCoord` c) allDirections

    isOk = \c -> m c /= None
    isReachable = \c -> m c /= Wall



-- player

player :: Direction -> Picture
player d = leftEye d & rightEye d & mouth & face
  where
    mouth = translated (-0.05) (-0.25) (polyline [(0, 0), (0.2, 0.06)])
    face = colored (lighter 0.2 yellow) (solidCircle 0.45) & solidCircle 0.45
    leftEye d = translated (-0.2) 0.1 (eye d)
    rightEye d = translated 0.2 0.1 (eye d)
    eye d = pupil d & colored white (solidCircle 0.15) & circle 0.15
    pupilDx = 0.07
    pupil U = translated 0 pupilDx (solidCircle 0.05)
    pupil D = translated 0 (-pupilDx) (solidCircle 0.05)
    pupil R = translated pupilDx 0 (solidCircle 0.05)
    pupil L = translated (-pupilDx) 0 (solidCircle 0.05)

-- resettable activity

reseteble :: Activity world -> Activity world
reseteble (Activity w0 handle draw) = Activity w0 handle' draw
  where
    handle' (KeyPress "R") w = w0
    handle' e w = handle e w

-- activity with start screen
data StartState world = StartScreen | Running world deriving Eq

withStartScreen :: Activity world -> Activity (StartState world)
withStartScreen (Activity w0 handle draw) = Activity w0' handle' draw'
  where
    w0' = StartScreen

    handle' (KeyPress " ") StartScreen = handle' (KeyPress " ") (Running w0)
    handle' _ StartScreen = StartScreen
    handle' e (Running w) = Running (handle e w)

    draw' StartScreen = scaled 3 3 (lettering "go??")
    draw' (Running w) = draw w


-- activity with undo
data WithUndo world = WithUndo world (List world)

withUndo :: Eq w => Activity w -> Activity (WithUndo w)
withUndo (Activity w0 handle draw) = Activity w0' handle' draw'
  where
    w0' = WithUndo w0 Empty

    handle' (KeyPress "U") s@(WithUndo _ Empty) = s
    handle' (KeyPress "U") (WithUndo _ (Elem w stack)) = WithUndo w stack
    handle' e s@(WithUndo w stack) = if w == w' then s else WithUndo w' (Elem w stack)
      where
        w' = handle e w

    draw' (WithUndo w _) = draw w

-- activity
data Activity world = Activity world (Event -> world -> world) (world -> Picture)

runActivity :: Activity world -> IO()
runActivity (Activity w0 handle draw) = activityOf w0 handle draw


-- sokoban
data AppState = S Direction Coord (List Coord) Integer deriving Eq

loadLevel :: Integer -> AppState
loadLevel level = S U start (initialBoxes m) level
  where
    MazeWithStart start m = nth mazes level

initialBoxes :: Maze -> List Coord
initialBoxes m = filterList isBox coords
  where
    isBox c = case m c of
      Box -> True
      _ -> False

handleEvent :: Event -> AppState -> AppState
handleEvent (KeyPress " ") s@(S dir player boxes level) | isPeremoha s = loadLevel (level + 1)
handleEvent _ s | isPeremoha s = s
handleEvent (KeyPress k) s = case k of
  "Up" -> moveIfPossible U s
  "Down" -> moveIfPossible D s
  "Right" -> moveIfPossible R s
  "Left" -> moveIfPossible L s
  _ -> s
handleEvent _ s = s

moveIfPossible :: Direction -> AppState -> AppState
moveIfPossible dir (S _ c boxes level) = state'
  where
    state' = if isPlayerMovable destination then S dir destination boxes' level else S dir c boxes level
    destination = calcCoord dir c
    boxes' = mapList (moveFromTo destination (calcCoord dir destination)) boxes
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


drawState :: AppState -> Picture
drawState s@(S _ _ _ level) | isPeremoha s = scaled 1 1 (lettering (if isLastLevel then sokobanDoneMsg else levelDoneMsg))
  where
    levelDoneMsg = [i|level #{level} of #{totalLevels} completed!|]
    sokobanDoneMsg = "whoohoo! you've completed all levels!"
    totalLevels = listLength mazes
    isLastLevel = level == totalLevels

drawState (S dir c boxes level) = atCoord c (player dir) & pictureOfBoxes boxes & pictureOfMaze (nthMaze level)

sokoban :: Activity AppState
sokoban = Activity (loadLevel 1) handleEvent drawState

isPeremoha :: AppState -> Bool
isPeremoha (S _ _ boxes level) = forall (isOnStorage (nthMaze level)) boxes

-- main

main :: IO ()
main = runActivity (reseteble (withStartScreen (withUndo sokoban)))



-- mazes 
type Maze = Coord -> Tile
data MazeWithStart = MazeWithStart Coord Maze

nthMaze :: Integer -> Maze
nthMaze n = m
  where
    MazeWithStart _ m = nth mazes n

mazes :: List MazeWithStart
mazes =
  MazeWithStart (C 1 1)       maze9 `Elem `
  MazeWithStart (C 0 0)       maze8 `Elem `
  MazeWithStart (C (-3) 3)    maze7 `Elem `
  MazeWithStart (C (-2) 4)    maze6 `Elem `
  MazeWithStart (C 0 1)       maze5 `Elem `
  MazeWithStart (C 1 (-3))    maze4 `Elem `
  MazeWithStart (C (-4) 3)    maze3 `Elem `
  MazeWithStart (C 0 1)       maze1 `Elem `
  Empty

extraMazes :: List MazeWithStart
extraMazes =
  MazeWithStart (C 1 (-3))    maze4'   `Elem`
  MazeWithStart (C 1 (-3))    maze4''  `Elem`
  MazeWithStart (C 1 1)       maze9'   `Elem`
  mazes

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
  | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
  | x == 0 && abs y < 4                     = Storage
  | x == -1 && (y == 0 || abs y == 2)       = Box
  | x == 1 && (abs y == 1 || abs y == 3)    = Box
  | x == (-2) &&  y == 1                    = Wall
  | otherwise                               = Ground

maze7 :: Maze
maze7 (C x y)
  | abs x > 4  || abs y > 4   = None
  | abs x == 4 || abs y == 4  = Wall
  | not (x == 2)  && y == 2   = Wall
  | not (x == -2)  && y == -1 = Wall
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
