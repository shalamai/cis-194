{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

-- list

data List a = Empty | Elem a (List a)

mapList ::  (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Elem a as) = Elem (f a) (mapList f as)

combine :: b -> (b -> a -> b) -> List a -> b
combine z _ Empty = z
combine z f (Elem a as) = f (combine z f as) a

append :: List a -> List a -> List a
append Empty b = b
append (Elem x xs) b = Elem x (append xs b)

filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList p (Elem a as) = if p a then Elem a (filterList p as) else filterList p as

exists :: (a -> Bool) -> List a -> Bool
exists p as = combine False (||) (mapList p as)

forall ::  (a -> Bool) -> List a -> Bool
forall p as = combine True (&&) (mapList p as)

-- fold21

fold21 :: a -> (a -> a -> a) -> (Integer -> a) -> a
fold21 z combine f = go (-10)
  where
    go n
      | n > 10 = z
      | otherwise = combine (f n) (go (n + 1))

-- area

data Direction = U | D | R | L
data Coord = Coord Integer Integer

coords :: List Coord
coords = fold21 Empty append (\r -> fold21 Empty append (\c -> Elem (Coord r c) Empty))

eqCoord :: Coord -> Coord -> Bool
eqCoord (Coord r c) (Coord r' c') = r == r' && c == c'

calcCoord :: Direction -> Coord -> Coord
calcCoord U (Coord x y) = Coord x (y + 1)
calcCoord D (Coord x y) = Coord x (y - 1)
calcCoord R (Coord x y) = Coord (x + 1) y
calcCoord L (Coord x y) = Coord (x - 1) y

-- maze

data Tile = Wall | Ground | Storage | Box | None

pictureOfMaze :: Picture
pictureOfMaze = combine blank (&) (mapList drawTileAt coords)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine blank (&) (mapList (\c -> atCoord c (drawTile Box)) cs)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))

drawTile :: Tile -> Picture
drawTile tile = case tile of
  Wall -> colored gray (solidRectangle 1 1)
  Ground -> thickRectangle 0.005 1 1
  Storage -> colored red (solidCircle 0.1) & thickRectangle 0.005 1 1
  Box -> colored white (rectangle 1 1) & colored (lighter 0.2 black) (solidRectangle 1 1)
  None -> blank

maze :: Coord -> Tile
maze (Coord x y)
  | abs x > 4 || abs y > 4 = None
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of
  Box -> Ground
  t -> t

mazeWithBoxes :: List Coord -> (Coord -> Tile)
mazeWithBoxes boxes = go
  where
    go :: Coord -> Tile
    go c | exists (eqCoord c) boxes = Box
    go c = noBoxMaze c

isOnStorage :: Coord -> Bool
isOnStorage c = case maze c of
  Storage -> True
  _ -> False

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) = translated (fromIntegral x) (fromIntegral y)

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
data StartState world = StartScreen | Running world

withStartScreen :: Activity world -> Activity (StartState world)
withStartScreen (Activity w0 handle draw) = Activity w0' handle' draw'
  where
    w0' = StartScreen

    handle' (KeyPress " ") StartScreen = handle' (KeyPress " ") (Running w0)
    handle' _ StartScreen = StartScreen
    handle' e (Running w) = Running (handle e w)

    draw' StartScreen = scaled 3 3 (lettering "go??")
    draw' (Running w) = draw w


-- activity
data Activity world = Activity world (Event -> world -> world) (world -> Picture)

runActivity :: Activity world -> IO()
runActivity (Activity w0 handle draw) = activityOf w0 handle draw


-- sokoban
data AppState = S Direction Coord (List Coord)

initialState :: AppState
initialState = S U (Coord 0 1) initialBoxes

initialBoxes :: List Coord
initialBoxes = filterList isBox coords
  where
    isBox c = case maze c of
      Box -> True
      _ -> False

handleEvent :: Event -> AppState -> AppState
handleEvent _ s | isPeremoha s = s
handleEvent (KeyPress k) s = case k of
  "Up" -> moveIfPossible U s
  "Down" -> moveIfPossible D s
  "Right" -> moveIfPossible R s
  "Left" -> moveIfPossible L s
  _ -> s
handleEvent _ s = s

moveIfPossible :: Direction -> AppState -> AppState
moveIfPossible dir (S _ c boxes) = state'
  where
    state' = if isPlayerMovable destination then S dir destination boxes' else S dir c boxes
    destination = calcCoord dir c
    boxes' = mapList (moveFromTo destination (calcCoord dir destination)) boxes
    moveFromTo from to c = if eqCoord from c then to else c

    isPlayerMovable to = case mazeWithBoxes boxes to of
      Ground -> True
      Storage -> True
      Box -> isBoxMovable (calcCoord dir to)
      _ -> False

    isBoxMovable to = case mazeWithBoxes boxes to of
      Ground -> True
      Storage -> True
      _ -> False


drawState :: AppState -> Picture
drawState s | isPeremoha s = scaled 3 3 (lettering "woohoooo!!!")
drawState (S dir c boxes) = atCoord c (player dir) & pictureOfBoxes boxes & pictureOfMaze

sokoban :: Activity AppState
sokoban = Activity initialState handleEvent drawState

isPeremoha :: AppState -> Bool
isPeremoha (S _ _ boxes) = forall isOnStorage boxes

-- main

main :: IO ()
main = runActivity (reseteble (withStartScreen sokoban))
