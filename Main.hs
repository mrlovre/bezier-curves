module Main where

import Graphics.UI.GLUT
import Display
import Listeners
import Data.IORef

initialSize :: Size
initialSize = Size 640 480

initialPosition :: Position
initialPosition = Position 100 100

main :: IO ()
main = do
    initialDisplayMode $= [RGBMode]
    initialWindowSize $= initialSize
    initialWindowPosition $= initialPosition
    _ <- getArgsAndInitialize
    _ <- createWindow "LAB 3 - Bezierove krivulje"
    points <- newIORef []
    selectedPoint <- newIORef (Nothing)
    displayCallback $= displayFunc points
    reshapeCallback $= Just reshapeFunc
    mouseCallback $= Just (mouseFunc selectedPoint points)
    keyboardCallback $= Just (keybFunc points)
    motionCallback $= Just (motionFunc selectedPoint points)
    mainLoop