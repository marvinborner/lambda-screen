module Screen
  ( Color(..)
  , Pos(..)
  , Area(..)
  , Image(..)
  , screenSplit
  ) where

data Color = Black | White | Grey
  deriving Show
data Pos = Pos Double Double
data Area = Square
  { x :: Pos
  , y :: Pos
  , c :: Color
  }

data Image = Screen [Image] | Pixel Color

screenSplit :: Area -> [Image] -> [Area]
screenSplit root@(Square { x = Pos x1 x2, c = color }) is =
  let perRow       = sqrt (fromIntegral $ length is)
      quadrantSize = (x2 - x1) / perRow

      go :: [Image] -> Double -> Double -> [Area]
      go is col row | col == perRow = go is 0 (row + 1)
      go (i : is) col row =
        Square { x = Pos (col * quadrantSize) ((col + 1) * quadrantSize)
               , y = Pos (row * quadrantSize) ((row + 1) * quadrantSize)
               , c = color
               }
          : go is (col + 1) row
      go _ _ _ = []
  in  go is 0 0
