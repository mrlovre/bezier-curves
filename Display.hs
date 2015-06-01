module Display where

import Graphics.UI.GLUT
import Bezier
import Data.IORef

green :: Color4 GLfloat
green = Color4 0 1 0 1

red :: Color3 GLfloat
red = Color3 1 0 0

blue :: Color3 GLfloat
blue = Color3 0 0 1

white :: Color3 GLfloat
white = Color3 1 1 1

black :: Color3 GLfloat
black = Color3 0 0 0

nDiv :: Num a => a
nDiv = 400

displayFunc :: IORef [Vertex2 GLdouble] -> DisplayCallback
displayFunc points_ = do
    points <- get points_
    clearColor $= green
    clear [ColorBuffer]
    matrixMode $= Modelview 0
    loadIdentity
    drawApproximationCurve points
    drawInterpolationCurve points
    drawControlLine points
    drawControlPoints points
    flush

reshapeFunc :: ReshapeCallback
reshapeFunc size@(Size w_ h_) = do
    let [w, h] = map fromIntegral [w_, h_]
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 w h 0
    postRedisplay Nothing

drawControlLine :: [Vertex2 GLdouble] -> IO ()
drawControlLine points = do
    color red
    renderPrimitive LineStrip $
        mapM_ vertex points

drawControlPoints :: [Vertex2 GLdouble] -> IO ()
drawControlPoints points = do
    color white
    mapM_ (fillCircle `flip` 5) points

drawInterpolationCurve :: [Vertex2 GLdouble] -> IO ()
drawInterpolationCurve points = do
    color black
    let nPoints = controlFromInterpolation points
    renderPrimitive LineStrip $
        mapM_ vertex (map (flip bezierPoint nPoints) [i / nDiv | i <- [0 .. nDiv]])

drawApproximationCurve :: [Vertex2 GLdouble] -> IO ()
drawApproximationCurve points = do
    color blue
    renderPrimitive LineStrip $
        mapM_ vertex (map (flip bezierPoint points) [i / nDiv | i <- [0 .. nDiv]])

fillCircle :: Vertex2 GLdouble -> GLdouble -> IO ()
fillCircle center@(Vertex2 x y) r = do
    let points = [Vertex2 (x + r * (cos phi)) (y - r * (sin phi)) | phi <- [k * pi / 8 | k <- [0 .. 16]] ]
    renderPrimitive TriangleFan $ do
        vertex center
        mapM_ vertex points