module Scheduler
  ( schedule
  , Strategy(..)
  ) where

import           Screen
import           Term

data Strategy = BFS | DFS

absolute :: Float -> Float
absolute n = if n >= 0 then n else -n

dfs :: Area -> Float -> Image -> [Area]
dfs root@(Square { x = Pos x1 x2 }) end _ | absolute (x2 - x1) < end =
  [root { c = Grey }]
dfs root end (Screen tl tr bl br) =
  (dfs (topLeft root) end tl)
    ++ (dfs (topRight root) end tr)
    ++ (dfs (bottomLeft root) end bl)
    ++ (dfs (bottomRight root) end br)
dfs root _ (Pixel c) = [root { c = c }]

schedule :: Strategy -> Float -> Image -> [Area]
-- schedule BFS = bfs
schedule DFS = dfs (Square { x = Pos (-1) 1, y = Pos (-1) 1, c = Grey })
