-- lexical.hs
-- Main Haskell module for lexical analysis

-- Allow exporting and importing symbols
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-} -- Allows better imports with capi as opposed to ccall

module Lexical where
  
import Foreign.C.Types
import Foreign.C.String

foreign import capi "lexicalli/interface.h get_char" get_char :: CInt -> CChar

run :: IO ()
run = do
  putChar (castCCharToChar (get_char 0))
  putChar (castCCharToChar (get_char 0))
  putChar (castCCharToChar (get_char 0))
  
foreign export ccall run :: IO ()