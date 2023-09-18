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
import Data.Array

-- Make certain C functions available to the linker
foreign import capi "lexicalli/interface.h get_char" get_char :: IO CChar
foreign import capi "lexicalli/interface.h put_token" put_token :: CString -> CInt -> CString -> CInt -> CInt 

-- Helper IO
-- Wrapper for getting the character via C
get_char_cast :: IO Char
get_char_cast = do
  c <- get_char
  return (castCCharToChar c)
-- Easy debug printing for returns from the main dfa function
print_str_int_char :: (String, Int, Char) -> IO ()
print_str_int_char (s, i, c) = do
  putStrLn s
  print i
  print c

-- Main DFA structure  
-- Generic type holding Sigma, delta, q_0, and F for a DFSA, needs types for its nonterminals and terminals
-- Called T, M, S, Z in the notes, I used the mathematical terms because I built this
--  before we covered it in class
-- Deviations from mathematics: Doesn't require a state alphabet as 
--  the state type is a generic parameter, so Q/K is removed from the tuple
type D_F_Automaton state term = ([term], (state->term->state), state, (state->Bool))

-- Alphabet building, currently some parts are unused
symbols = "+-*/{}()=!<>,"
digits = "1234567890"
letters = "zxcvbnmasdfghjklqwertyuiopASDFGHJKLZXCVBNMQWERTYUIOP"
whitespace = "\n\t "
end_file = '\255'
alphabet = symbols ++ digits ++ letters ++ whitespace ++ [end_file]


-- Structures and subroutines requiring modification later as I add more to the language
reserved_words = ["CLASS", "VAR", "CONST"]
-- Note: the end states are just rows of their number because the value doesn't matter
--                L, D, *, /, =, <, ws, e, semi
state_matrix = [[ 5, 3, 2, 7, 11, 14, 0, 1, 17 ],
                [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ], -- Error state
                [ 18, 18, 18, 18, 18, 18, 18, 1, 1 ],
                [ 1, 3, 4, 4, 4, 4, 4, 1, 4 ],
                [ 4, 4, 4, 4, 4, 4, 4, 4, 4 ], -- Integer state
                [ 5, 5, 6, 6, 6, 6, 6, 1, 6 ],
                [ 6, 6, 6, 6, 6, 6, 6, 6, 6 ], -- Var state
                [ 10, 10, 8, 10, 10, 10, 10, 1, 1 ],
                [ 8, 8, 9, 8, 8, 8, 8, 8, 8 ], -- /*
                [ 8, 8, 8, 20, 8, 8, 8, 8, 8 ], -- intermediate comma
                [ 10, 10, 10, 10, 10, 10, 10, 10, 10 ], -- Division operator state
                [ 12, 12, 1, 1, 13, 1, 12, 1, 1 ], 
                [ 12, 12, 12, 12, 12, 12, 12, 12, 12 ], -- Assignment operator
                [ 13, 13, 13, 13, 13, 13, 13, 13, 13 ], -- Comparison operator
                [ 15, 15, 1, 1, 16, 15, 15, 1, 1 ],
                [ 15, 15, 15, 15, 15, 15, 15, 15, 15 ], -- < Operator 
                [ 19, 19, 1, 1, 1, 1, 19, 1, 1 ],
                [ 17, 17, 17, 17, 17, 17, 17, 17, 17 ], -- semicolon
                [ 18, 18, 18, 18, 18, 18, 18, 18, 18 ], -- Multiplication operator
                [ 19, 19, 19, 19, 19, 19, 19, 19, 19 ], -- <= Operator   
                [ 0,  0,  0,  0,  0,  0,  0,  1,  0]]
end_states = [ 1, 4, 6, 10, 12, 13, 15, 17, 18, 19 ]
skip_states = [ 0, 8, 9, 20 ]
dump_states = [ 0, 8, 9, 20 ]
state_transition :: Int -> Char -> Int
state_transition s c | (elem c letters)    = state_matrix!!s!!0
                     | (elem c digits)     = state_matrix!!s!!1
                     | (c == '*')          = state_matrix!!s!!2
                     | (c == '/')          = state_matrix!!s!!3
                     | (c == '=')          = state_matrix!!s!!4
                     | (c == '<')          = state_matrix!!s!!5
                     | (elem c whitespace) = state_matrix!!s!!6
                     | (c == ';')          = state_matrix!!s!!8 
                     | otherwise           = state_matrix!!s!!7

-- Unchanging helper subroutines
is_end :: Int -> Bool
is_end x = elem x end_states
dump_token :: Int -> Bool
dump_token s = elem s dump_states

-- Main DFA scanner, scans character by character and returns the newest token
-- Function to check for valid grammar from an input automaton, a partial token, and a dump token func
-- Returns the next list of terminals denoting a token, its class, and the start token to resume parsing on
scan :: D_F_Automaton state term -> [term] -> (IO term) -> (state -> Bool) -> IO ([term], state, term)
scan (sigma, delta, s, f) token gt dump = do
  t <- gt
  
  --  Get the new state based on leftover token from last parse
  let st = delta s t
  -- Check for valid end-of-token, otherwise recurse with new state and slightly longer partial token
  if f st then return (token, st, t)
  else do
    if dump st then do -- Flush the entire token so far
      ret <- (scan (sigma, delta, st, f) [] gt dump) 
      return ret
    else do
      ret <- (scan (sigma, delta, st, f) (token ++ [t]) gt dump) 
      return ret

-- Loop calling DFA scanner and writing tokens to the intermediate file
-- While running this function my computer completely filled with bees.
-- I think they must have a hive inside the log, because it wouldn't
--  make sense for them to have a hive inside the recursion.
-- (they couldn't store any honey in the comb beyond depth 1)
log_and_recurse :: (String, Int, Char) -> IO () 
log_and_recurse (token, cls, ch) = do
  print_str_int_char (token, cls, ch) -- Debug
  putStrLn ""
  
  -- TODO Error checking based on current token and character
  if cls == 1 then putStrLn "Lexicalli: ERROR"
  else do
    -- Write token and class to token file
  
    -- Continue analysis
    if ch == end_file then putStrLn "Lexicalli: Success" -- file ended and we're in a successful state
    else do
      -- Resume the parse in the state from the character
      -- The reason for this conditional is that I don't want a leading whitespace
      --  in my next token
      if elem ch whitespace then do
        tok <- scan (alphabet, state_transition, 0, is_end) [] get_char_cast dump_token
        log_and_recurse tok
      else do
        tok <- scan (alphabet, state_transition, (state_transition 0 ch), is_end) [ch] get_char_cast dump_token
        log_and_recurse tok
  
-- Called by C
run_scanner :: IO ()
run_scanner = do
  -- Get the first token
  tok1 <- scan (alphabet, state_transition, 0, is_end) [] get_char_cast dump_token
  
  -- Continue the token scan
  log_and_recurse tok1
  
-- Tell the compiler to generate C headers called "stubs" to interface
-- The linker will take those stubs and the object files from compiled Haskell code and package them together
--  for the final app
foreign export ccall run_scanner :: IO ()