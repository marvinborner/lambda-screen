module Screen
  ( parse
  , Pixel(..)
  , Pos(..)
  , Area(..)
  , topLeft
  , topRight
  , bottomLeft
  , bottomRight
  ) where

data Pixel = Black | White | Grey
data Pos = Pos Float Float
data Area = Square
  { x :: Pos
  , y :: Pos
  }

topLeft :: Area -> Area
topLeft (Square { x = Pos x1 x2, y = Pos y1 y2 }) =
  let xPos = Pos x1 (x1 + (x2 - x1) / 2)
      yPos = Pos (y1 + (y2 - y1) / 2) y2
  in  Square xPos yPos

topRight :: Area -> Area
topRight (Square { x = Pos x1 x2, y = Pos y1 y2 }) =
  let xPos = Pos (x1 + (x2 - x1) / 2) x2
      yPos = Pos (y1 + (y2 - y1) / 2) y2
  in  Square xPos yPos

bottomLeft :: Area -> Area
bottomLeft (Square { x = Pos x1 x2, y = Pos y1 y2 }) =
  let xPos = Pos x1 (x1 + (x2 - x1) / 2)
      yPos = Pos y1 (y1 + (y2 - y1) / 2)
  in  Square xPos yPos

bottomRight :: Area -> Area
bottomRight (Square { x = Pos x1 x2, y = Pos y1 y2 }) =
  let xPos = Pos (x1 + (x2 - x1) / 2) x2
      yPos = Pos y1 (y1 + (y2 - y1) / 2)
  in  Square xPos yPos

data Term = Abs Int Term | App Term Term | Var Int

data HTerm = HAbs (HTerm -> HTerm) | HApp HTerm HTerm | HVar Int

parse :: String -> Term
parse _ = Abs 0 (Abs 1 (Var 0))
