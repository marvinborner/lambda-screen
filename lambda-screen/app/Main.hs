module Main where

import           Graphics.Rendering.OpenGL
                                         hiding ( Color )
import           Graphics.UI.GLUT        hiding ( Color )
import           Scheduler
import           Screen
import           Term

resolveColor :: Color -> Color3 GLfloat
resolveColor Black = Color3 0.0 0.0 0.0 :: Color3 GLfloat
resolveColor White = Color3 1.0 1.0 1.0 :: Color3 GLfloat
resolveColor Grey  = Color3 0.6 0.6 0.6 :: Color3 GLfloat

square :: Area -> IO ()
square (Square { x = Pos x1 x2, y = Pos y1 y2, c = c }) =
  renderPrimitive Quads $ do
    color $ resolveColor c
    vertex (Vertex3 x1 y1 0 :: Vertex3 GLfloat)
    vertex (Vertex3 x1 y2 0 :: Vertex3 GLfloat)
    vertex (Vertex3 x2 y2 0 :: Vertex3 GLfloat)
    vertex (Vertex3 x2 y1 0 :: Vertex3 GLfloat)

display :: Term -> IO ()
display t = do
  clear [ColorBuffer]
  mapM_
    (\s -> do
      square s
      -- flush -- TODO: chunking and not on main thread!
    )
    (schedule DFS 0.001 (render t))
  flush

initial :: Term
initial = Abs (App (App (App (App (Idx 0) b) b) b) b)
  where b = Abs (Abs (Idx 0))

main :: IO ()
main = do
  inp <- getContents
  let t       = parse inp
  let program = App t initial
  print program
  (name, _) <- getArgsAndInitialize
  createWindow name
  displayCallback $= (display program)
  mainLoop
