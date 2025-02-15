module Main where

import           GHC.Wasm.Prim
import           Screen
import           Term

type JSFunction = JSVal

foreign import javascript safe "wrapper"
  mkCont :: IO () -> IO JSFunction

foreign export javascript "onRender" onRender :: Double -> JSString -> IO ()

foreign import javascript unsafe "render($1, $2, $3, $4, $5, $6)"
  js_draw :: Double -> Double -> Double -> Double -> JSString -> JSFunction -> IO ()

foreign import javascript unsafe "flush()"
  flush :: IO ()

main :: IO ()
main = error "uwu"

absolute :: Double -> Double
absolute n = if n >= 0 then n else -n

dfs :: Area -> Double -> Image -> IO ()
dfs root@(Square { x = Pos x1 x2, y = Pos y1 y2 }) lim p
  | absolute (x2 - x1) < lim = do
    cont <- mkCont $ dfs root (lim / 5) p
    js_draw x1 x2 y1 y2 (toJSString $ show Grey) cont
dfs (Square { x = Pos x1 x2, y = Pos y1 y2 }) _ (Pixel color) = do
  cont <- mkCont $ pure ()
  js_draw x1 x2 y1 y2 (toJSString $ show color) cont
dfs root lim (Screen ts) =
  mapM_ (\(t, sub) -> dfs sub lim t) (zip ts (screenSplit root ts))

initial :: Term
initial = Abs (App (App (App (App (Idx 0) b) b) b) b)
  where b = Abs (Abs (Idx 0))

onRender :: Double -> JSString -> IO ()
onRender size input = do
  let term = parse $ fromJSString input
  let root = Square { x = Pos 0 size, y = Pos 0 size, c = Grey }
  dfs root 10 $ render $ App term initial
  flush
