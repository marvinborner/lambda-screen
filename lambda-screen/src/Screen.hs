module Screen
  ( Color(..)
  , Pos(..)
  , Area(..)
  , Image(..)
  , topLeft
  , topRight
  , bottomLeft
  , bottomRight
  ) where

data Color = Black | White | Grey
data Pos = Pos Float Float
data Area = Square
  { x :: Pos
  , y :: Pos
  , c :: Color
  }

data Image = Screen Image Image Image Image | Pixel Color

topLeft :: Area -> Area
topLeft (Square { x = Pos x1 x2, y = Pos y1 y2, c = color }) =
  let xPos = Pos x1 (x1 + (x2 - x1) / 2)
      yPos = Pos (y1 + (y2 - y1) / 2) y2
  in  Square xPos yPos color

topRight :: Area -> Area
topRight (Square { x = Pos x1 x2, y = Pos y1 y2, c = color }) =
  let xPos = Pos (x1 + (x2 - x1) / 2) x2
      yPos = Pos (y1 + (y2 - y1) / 2) y2
  in  Square xPos yPos color

bottomLeft :: Area -> Area
bottomLeft (Square { x = Pos x1 x2, y = Pos y1 y2, c = color }) =
  let xPos = Pos x1 (x1 + (x2 - x1) / 2)
      yPos = Pos y1 (y1 + (y2 - y1) / 2)
  in  Square xPos yPos color

bottomRight :: Area -> Area
bottomRight (Square { x = Pos x1 x2, y = Pos y1 y2, c = color }) =
  let xPos = Pos (x1 + (x2 - x1) / 2) x2
      yPos = Pos y1 (y1 + (y2 - y1) / 2)
  in  Square xPos yPos color
