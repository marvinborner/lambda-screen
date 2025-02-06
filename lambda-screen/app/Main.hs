module Main where

import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT
import           Screen

resolvePixel :: Pixel -> Color3 GLfloat
resolvePixel Black = Color3 0.0 0.0 0.0 :: Color3 GLfloat
resolvePixel White = Color3 1.0 1.0 1.0 :: Color3 GLfloat
resolvePixel Grey  = Color3 0.6 0.6 0.6 :: Color3 GLfloat

square :: Area -> Pixel -> IO ()
square (Square { x = Pos x1 x2, y = Pos y1 y2 }) pixel =
  renderPrimitive Quads $ do
    color $ resolvePixel pixel
    vertex (Vertex3 x1 y1 0 :: Vertex3 GLfloat)
    vertex (Vertex3 x1 y2 0 :: Vertex3 GLfloat)
    vertex (Vertex3 x2 y2 0 :: Vertex3 GLfloat)
    vertex (Vertex3 x2 y1 0 :: Vertex3 GLfloat)

display :: IO ()
display = do
  clear [ColorBuffer]
  square (topLeft $ Square { x = Pos (-1) 1, y = Pos (-1) 1 }) Grey
  flush

main :: IO ()
main = do
  let t = parse "\\0"
  (name, _) <- getArgsAndInitialize
  createWindow name
  displayCallback $= display
  mainLoop
