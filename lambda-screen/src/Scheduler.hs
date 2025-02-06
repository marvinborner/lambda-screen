module Scheduler
  ( schedule
  , Strategy(..)
  ) where

import           Data.Sequence                  ( Seq(..)
                                                , ViewL(..)
                                                , (|>)
                                                )
import qualified Data.Sequence                 as Seq
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

-- TODO: Could streams be interesting for parallel reduction via threading pools?
-- dfs' :: Monad m => Area -> Float -> Image -> Stream (Of Area) m ()
-- dfs' root@Square { x = Pos x1 x2 } end _ | absolute (x2 - x1) < end =
--   S.yield $ root { c = Grey }
-- dfs' root end (Screen tl tr bl br) = do
--   dfs' (topLeft root)     end tl
--   dfs' (topRight root)    end tr
--   dfs' (bottomLeft root)  end bl
--   dfs' (bottomRight root) end br
-- dfs' root _ (Pixel c) = S.yield $ root { c = c }

-- TODO: is this correct?
bfs :: Seq (Area, Image) -> Float -> [Area] -> [Area]
bfs queue end result = case Seq.viewl queue of
  EmptyL                  -> result
  (area, Pixel c) :< rest -> bfs rest end (area { c = c } : result)
  (area@(Square { x = Pos x1 x2 }), img@(Screen tl tr bl br)) :< rest
    | absolute (x2 - x1) < end -> bfs rest end (area { c = Grey } : result)
    | otherwise                -> bfs queue' end result
   where
    queue' =
      rest
        |> (topLeft area    , tl)
        |> (topRight area   , tr)
        |> (bottomLeft area , bl)
        |> (bottomRight area, br)

schedule :: Strategy -> Float -> Image -> [Area]
schedule BFS end image = bfs
  (Seq.singleton (Square { x = Pos (-1) 1, y = Pos (-1) 1, c = Grey }, image))
  end
  []
schedule DFS end image =
  dfs (Square { x = Pos (-1) 1, y = Pos (-1) 1, c = Grey }) end image
