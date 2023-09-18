-- lexical.hs
-- Main Haskell module for lexical analysis

-- Allow exporting and importing symbols
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-} -- Allows better imports with capi as opposed to ccall

module Lexical where
  
import Foreign.C.Types
import Foreign.C.String
import Data.List
import Data.Maybe
import Data.Char (ord)

foreign import capi "lexicalli/interface.h get_char" get_char :: IO CChar
foreign import capi "lexicalli/interface.h put_token" put_token :: CString -> CInt -> CString -> CInt -> CInt 

get_char_cast :: IO Char
get_char_cast = do
  c <- get_char
  return (castCCharToChar c)

symbols = "+-*/{}()=!<>,"
digits = "1234567890"
letters = "zxcvbnmasdfghjklqwertyuiopASDFGHJKLZXCVBNMQWERTYUIOP"
whitespace = "\n\t "
alphabet = symbols ++ digits ++ letters ++ whitespace

state_matrix = [[ 5, 3, 1, 7, 11, 14, 0, 1 ],
                [ 1, 1, 1, 1, 1, 1, 1, 1 ], -- Error state
                [ 2, 2, 2, 2, 2, 2, 2, 1 ], -- Multiplication operator state
                [ 4, 3, 4, 4, 4, 4, 4, 1 ],
                [ 4, 4, 4, 4, 4, 4, 4, 1 ], -- Integer state
                [ 5, 5, 6, 6, 6, 6, 6, 1 ],
                [ 6, 6, 6, 6, 6, 6, 6, 1 ], -- Var state
                [ 10, 10, 8, 10, 10, 10, 10, 1 ],
                [ 8, 8, 9, 8, 8, 8, 8, 8 ], -- Comment state
                [ 8, 8, 8, 0, 8, 8, 8, 1 ],
                [ 10, 10, 10, 10, 10, 10, 10, 1 ], -- Division operator state
                [ 12, 12, 12, 12, 13, 12, 12, 1 ], 
                [ 12, 12, 12, 12, 12, 12, 12, 1 ], -- Assignment operator
                [ 13, 13, 13, 13, 13, 13, 13, 1 ], -- Comparison operator
                [ 15, 15, 15, 15, 16, 15, 15, 1 ],
                [ 15, 15, 15, 15, 15, 15, 15, 1 ], -- < Operator 
                [ 16, 16, 16, 16, 16, 16, 16, 1 ]] -- <= Operator
                
                
end_states = [ 1, 2, 4, 6, 10, 12, 13, 15, 16 ]
is_end :: Int -> Bool
is_end x = elem x end_states

state_transition :: Int -> Char -> Int
state_transition s c | (elem c letters)    = state_matrix!!s!!0
                     | (elem c digits)     = state_matrix!!s!!1
                     | (c == '*')          = state_matrix!!s!!2
                     | (c == '/')          = state_matrix!!s!!3
                     | (c == '=')          = state_matrix!!s!!4
                     | (c == '<')          = state_matrix!!s!!5
                     | (elem c whitespace) = state_matrix!!s!!6
                     | otherwise           = state_matrix!!s!!7



-- Generic type holding Q, Sigma, delta, q_0, and F for a DFSA, needs types for its nonterminals and terminals
-- Called K, T, M, S, Z in the notes, I used the mathematical terms because I built this
--  before we covered it in class
type D_F_Automaton state term = ([state], [term], (state->term->state), state, (state->Bool))
-- Function to check for valid grammar from an input automaton, a partial token, and a terminal callback
-- Returns the next list of terminals denoting a token, its class, and the start token to resume parsing on
scan :: D_F_Automaton state term -> [term] -> (IO term) -> IO ([term], state, term)
scan (q, sigma, delta, s, f) token gt = do
  -- Get the terminal at the head of the input stream, character by character
  t <- gt
  
  -- Get a new state based on current state and terminal
  let st = delta s t 
  
  -- Check for valid end-of-token, otherwise recurse with new state and slightly longer partial token
  if f st then return (token, s, t)
  else do
    ret <- (scan (q, sigma, delta, st, f) (token ++ [t]) gt)
    return ret

run_scanner :: IO ()
run_scanner = do
  print (state_matrix!!1!!1)
  x <- get_char_cast
  print x
  a <- get_char
  b <- get_char
  c <- get_char
  putChar (castCCharToChar a)
  putChar (castCCharToChar b)
  putChar (castCCharToChar c)
  j <- newCString "Joe"
  f <- newCString "5"
  print (put_token j (CInt 0) f (CInt 1))
  
foreign export ccall run_scanner :: IO ()