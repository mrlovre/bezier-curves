{-# LANGUAGE ForeignFunctionInterface #-}
module CMatrixInversion where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
import System.IO.Unsafe

foreign import ccall unsafe "\"matrix_inversion.h\" invert_matrix"
    cInvertMatrix :: CInt -> Ptr CDouble -> IO (Ptr CDouble)

type Matrix = [[Double]]

matrixInverse :: Matrix -> Matrix
matrixInverse matrix = let
    n = length matrix
    n_ = fromIntegral n
    values = concatMap (map realToFrac) matrix
    arrSize = length values
    in unsafePerformIO $ do
        numsPtr <- withArray values $ \valuesPtr ->
            cInvertMatrix n_ valuesPtr
        nums <- peekArray arrSize numsPtr
        return $ [[realToFrac $ nums !! (i * n + j) | j <- [0 .. pred n]] | i <- [0 .. pred n]]
