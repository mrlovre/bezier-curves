module Bezier where

import Graphics.UI.GLUT hiding (Matrix)
import qualified CMatrixInversion as CMI

binomial :: Int -> Int -> Int
binomial = doWork 1 1 where
    doWork rn rd _ 0 = rn `div` rd
    doWork _  _  0 _ = 0
    doWork rn rd n k = doWork (rn * n) (rd * k) (n - 1) (k - 1)

controlFromInterpolation :: [Vertex2 GLdouble] -> [Vertex2 GLdouble]
controlFromInterpolation [] = []
controlFromInterpolation iPoints = let
    n = (pred . length) iPoints
    mPoints = map (\(Vertex2 x_ y_) -> map realToFrac [x_, y_]) iPoints
    cPoints = bernsteinMatricesInverses !! n `matrixMult` mPoints
    makeVertex [x, y] = Vertex2 x_ y_ where
        [x_, y_] = map realToFrac [x, y]
    in map makeVertex cPoints

type Matrix = [[Double]]

bernsteinMatricesInverses :: [Matrix]
bernsteinMatricesInverses = [CMI.matrixInverse m | m <- [bernsteinMatrix n | n <- [0 ..]]]

bernsteinMatrix :: Int -> Matrix
bernsteinMatrix n = [[bernsteinCoefficient n k t | k <- ks] | t <- ts] where
    ks = [0 .. n]
    n_ = fromIntegral n
    ts = [i / n_ | i <- [0 .. n_]]

bezierPoint :: Double -> [Vertex2 GLdouble] -> Vertex2 GLdouble
bezierPoint t_ controls = let
    t = realToFrac t_
    n = (pred . length) controls
    asc = [0 .. n]
    desc = reverse asc
    bins = map (binomial n) [0 .. n]
    coeffs = zipWith3 calcCoeff bins asc desc
    calcCoeff k_ p_ q_ = (k * (t ** p) * ((1 - t) ** q)) where
        [k, p, q] = map realToFrac [k_, p_, q_]
    update (Vertex2 x1 y1) (s, Vertex2 x2 y2) = Vertex2 (x1 + s * x2) (y1 + s * y2)
    in foldl update (Vertex2 0 0) (zip coeffs controls)


bernsteinCoefficient :: Int -> Int -> Double -> Double
bernsteinCoefficient n_ k_ t = let
    [n, k] = map realToFrac [n_, k_]
    in fromIntegral (n_ `binomial` k_) * (t ** k) * ((1 - t) ** (n - k))

matrixMult :: Matrix -> Matrix -> Matrix
matrixMult [] [] = []
matrixMult m1 m2 = let
    r1 = length m1
    r2 = length m2
    c1 = length $ head m1
    c2 = length $ head m2
    in if c1 /= r2
        then error "Incompatible matrix dimensions." 
        else [[sum [(m1 !! i !! k) * (m2 !! k !! j) | k <- [0 .. r2 - 1]] | j <- [0 .. c2 - 1]] | i <- [0 .. r1 - 1]]

matrixInverse :: Matrix -> Matrix
matrixInverse [] = []
matrixInverse [[a]] = [[a]]
matrixInverse m = let
    r = length m
    c = length $ head m
    in scaleMatrix (1 / determinant m) (transposeMatrix [[(-1) ** fromIntegral (i + j) * determinant (submatrix i j m) | j <- [0 .. c - 1]] | i <- [0 .. r - 1]])

determinant :: Matrix -> Double
determinant [] = 0
determinant [[a]] = a
determinant [[a, b], [c, d]] = a * d - b * c
determinant m = let
    r = length m
    c = length $ head m
    alternating = map ((-1) **) [0 ..]
    firstRow = zip (repeat 0) [0 .. c - 1]
    in if r /= c
        then error "Not square matrix."
        else sum (zipWith (*) (map (\(dr, dc) -> m !! dr !! dc * determinant (submatrix dr dc m)) firstRow) alternating)

submatrix :: Int -> Int -> Matrix -> Matrix
submatrix _ _ [] = []
submatrix dr dc m = let
    r = length m
    c = length $ head m
    in [[m !! i !! j | j <- [0 .. dc - 1] ++ [dc + 1 .. c - 1]] | i <- [0 .. dr - 1] ++ [dr + 1 .. r - 1]]

scaleMatrix :: Double -> Matrix -> Matrix
scaleMatrix _ [] = []
scaleMatrix s m = let
    r = length m
    c = length $ head m
    in [[(m !! i !! j) * s | j <- [0 .. c - 1]] | i <- [0 .. r - 1]]

transposeMatrix :: Matrix -> Matrix
transposeMatrix [] = []
transposeMatrix m = let
    r = length m
    c = length $ head m
    in [[m !! j !! i | j <- [0 .. c - 1]] | i <- [0 .. r - 1]]