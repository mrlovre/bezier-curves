module Listeners where

import           Data.Char
import           Data.IORef
import           Data.List
import           Data.Ord         hiding (Down)
import           Graphics.UI.GLUT

rSquared :: Double
rSquared = 25

escapeKey :: Char
escapeKey = chr 27

mouseFunc :: IORef (Maybe Int) -> IORef [Vertex2 GLdouble] -> MouseCallback
mouseFunc selectedPoint_ points_ button state (Position x_ y_) = do
    points <- get points_
    let [x, y] = map realToFrac [x_, y_]
        closestPointInd
            | null points = Nothing
            | otherwise = case filter ((<= rSquared) . dist . snd) (zip [0 ..] points) of
                [] -> Nothing
                s -> Just $ (fst . head . sortBy (comparing snd)) s
        dist (Vertex2 x1_ y1_) = (x1 - x) ** 2 + (y1 - y) ** 2 where
            [x1, y1] = map realToFrac [x1_, y1_]
    case (state, button) of
        (Down, RightButton) -> points_ $= points ++ [Vertex2 (realToFrac x) (realToFrac y)]
        (_, LeftButton) -> case state of
            Down -> selectedPoint_ $= closestPointInd
            Up -> selectedPoint_ $= Nothing
        _ -> return ()
    postRedisplay Nothing

motionFunc :: IORef (Maybe Int) -> IORef [Vertex2 GLdouble] -> MotionCallback
motionFunc selectedPoint_ points_ (Position x_ y_) = do
    ind <- get selectedPoint_
    let [x, y] = map realToFrac [x_, y_]
    case ind of
        Just i -> do
            points_ $~ updateList (i, Vertex2 x y)
            postRedisplay Nothing
        Nothing -> return ()

updateList :: (Int, a) -> [a] -> [a]
updateList (ind, val) list =
    [if i == ind then val else v | (i, v) <- zip [0 ..] list]

keybFunc :: IORef [Vertex2 GLdouble] -> KeyboardCallback
keybFunc points_ key _
    | key == escapeKey = do
        points_ $= []
        postRedisplay Nothing
    | otherwise = return ()

