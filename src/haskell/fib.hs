-- fib.hs
-- Test module from HaskellWiki for the FFI

{-# LANGUAGE ForeignFunctionInterface #-}

module Safe where

import Foreign.C.Types

fibonacci :: Int -> Int
fibonacci n = fibs !! (n - 1)
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci_hs :: CInt -> CInt
fibonacci_hs = fromIntegral . fibonacci . fromIntegral

foreign export ccall fibonacci_hs :: CInt -> CInt