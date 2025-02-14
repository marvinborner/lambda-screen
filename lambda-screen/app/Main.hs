module Main where

import           Control.Concurrent.ParallelIO.Global
import           GHC.Wasm.Prim
import           Screen
import           Term

foreign export javascript "onRender" onRender :: Double -> JSString -> IO ()

foreign import javascript unsafe "render($1, $2, $3, $4, $5)"
  js_draw :: Double -> Double -> Double -> Double -> JSString -> IO ()

foreign import javascript safe "flush()"
  flush :: IO ()

main :: IO ()
main = error "uwu"

absolute :: Double -> Double
absolute n = if n >= 0 then n else -n

dfs :: Area -> Image -> IO ()
dfs (Square { x = Pos x1 x2, y = Pos y1 y2 }) _ | absolute (x2 - x1) < 1 =
  js_draw x1 x2 y1 y2 (toJSString $ show Grey)
dfs root (Screen tl tr bl br) = do
  dfs (topLeft root)     tl
  dfs (topRight root)    tr
  dfs (bottomLeft root)  bl
  dfs (bottomRight root) br

dfs (Square { x = Pos x1 x2, y = Pos y1 y2 }) (Pixel c) =
  js_draw x1 x2 y1 y2 (toJSString $ show c)

initial :: Term
initial = Abs (App (App (App (App (Idx 0) b) b) b) b)
  where b = Abs (Abs (Idx 0))

onRender :: Double -> JSString -> IO ()
onRender size input = do
  let term = parse $ fromJSString input
  let root = Square { x = Pos 0 size, y = Pos 0 size, c = Grey }
  dfs root $ render $ App term initial
  flush
