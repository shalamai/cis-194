{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

-- domain

data Tile = Wall | Ground | Storage | Box | None
data Direction = U | D | R | L
data Coord = Coord Integer Integer
data AppState = S Direction Coord

-- maze

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (drawTileAt . Coord r))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go 11 = blank
    go n = something n & go (n + 1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (maze c))

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

-- state transition

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) = translated (fromIntegral x) (fromIntegral y)

calcCoord :: Direction -> Coord -> Coord
calcCoord U (Coord x y) = Coord x (y + 1)
calcCoord D (Coord x y) = Coord x (y - 1)
calcCoord R (Coord x y) = Coord (x + 1) y
calcCoord L (Coord x y) = Coord (x - 1) y

moveIfPossible :: Direction -> AppState -> AppState
moveIfPossible dir (S _ c) = S dir (if not (isObstacle destination) then destination else c)
  where
    destination = calcCoord dir c
    isObstacle c = case maze c of
      Ground -> False
      Storage -> False
      _ -> True

-- resettable

resetableActivityOf :: world -> (Event -> world -> world) -> (world -> Picture) -> IO ()
resetableActivityOf initialState eventHandler = activityOf initialState resettableHandler
  where
    resettableHandler (KeyPress "R") = const initialState
    resettableHandler e = eventHandler e

-- main

initialState :: AppState
initialState = S U (Coord 0 1)

handleEvent :: Event -> AppState -> AppState
handleEvent (KeyPress k) = case k of
  "Up" -> moveIfPossible U
  "Down" -> moveIfPossible D
  "Right" -> moveIfPossible R
  "Left" -> moveIfPossible L
  _ -> id
handleEvent _ = id

drawState :: AppState -> Picture
drawState (S dir c) = atCoord c (player dir) & pictureOfMaze

main :: IO ()
main = resetableActivityOf initialState handleEvent drawState
