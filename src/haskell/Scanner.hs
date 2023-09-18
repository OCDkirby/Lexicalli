-- Scanner.hs
-- Lexical analysis automaton

-- Compiler macros to allow exporting and importing symbols
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-} -- Allows better imports with capi as opposed to ccall

module Scanner where

import Foreign.C.Types
import Foreign.C.String
import Data.Char (ord)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (when)
import DFA

-- Make certain C functions available to the linker
foreign import capi "lexicalli/interface.h get_char" get_char :: IO CChar
foreign import capi "lexicalli/interface.h put_token" put_token :: CString -> CInt -> IO ()

-- Helper IO
-- Wrapper for getting the character via C
getCharCast :: IO Char
getCharCast = castCCharToChar <$> get_char

-- Column in state transition matrix for each character
getCharType :: Char -> Int
getCharType c | c `elem` letters          = 0
              | c `elem` digits           = 1
              | c == '*'                  = 2
              | c == '/'                  = 3
              | c == '='                  = 4
              | c == '<'                  = 5
              | c `elem` whitespace       = 6
              | c == ';' || c == end_file = 8 
              | c == '+'                  = 9
              | c == '-'                  = 10
              | c == '{'                  = 11
              | c == '}'                  = 12
              | c == ','                  = 13
              | c == '('                  = 14
              | c == ')'                  = 15
              | c == '>'                  = 16
              | c == '!'                  = 17
              | c == '['                  = 18
              | c == ']'                  = 19
              | otherwise                 = 7 -- Unexpected/illegal char

-- English name for each character type
getTokenName :: Int -> String
getTokenName cls | cls ==  0 = "letter"
                 | cls ==  1 = "digit"
                 |  cls ==  2
                 || cls ==  3
                 || cls ==  9
                 || cls == 10 = "arithmetic"
                 | cls ==  4 = "="
                 |  cls ==  5
                 || cls == 16
                 || cls == 17 = "comparison"
                 | cls ==  6 = "whitespace"
                 | cls ==  7 = "error"
                 | cls ==  8 = "semicolon"
                 | cls == 11 = "{"
                 | cls == 12 = "}"
                 | cls == 13 = "comma"
                 | cls == 14 = "("
                 | cls == 15 = ")"
                 | cls == 18 = "["
                 | cls == 19 = "]"

-- Print the non-error possibilities for a single row of the state table
printRow :: [Int] -> Int -> Bool -> Bool -> IO ()
printRow [st] counter saidArithmetic saidComparison = do
  let sa = saidArithmetic || getTokenName counter == "arithmetic"
  let sc = saidComparison || getTokenName counter == "comparison"
  when (st /= 1 
    && ((not saidArithmetic || (saidArithmetic && getTokenName counter /= "arithmetic"))
     || (not saidComparison || (saidComparison && getTokenName counter /= "comparison"))))
    $ putStr $ getTokenName counter ++ ","

printRow (st:rest) counter saidArithmetic saidComparison = do
  let sa = saidArithmetic || getTokenName counter == "arithmetic"
  let sc = saidComparison || getTokenName counter == "comparison"
  when (st /= 1 
    && (not saidArithmetic || (saidArithmetic && getTokenName counter /= "arithmetic"))
    && (not saidComparison || (saidComparison && getTokenName counter /= "comparison")))
    $ putStr $ getTokenName counter ++ ", "
  printRow rest (counter + 1) sa sc

-- Get a state from a chain of tokens by backtracking to the first 
backtrackStates :: String -> Char -> Int
backtrackStates [] tok = stateTransition 0 tok
backtrackStates (prev:rest) tok = stateTransition (backtrackStates rest prev) tok

-- Print the error explanation
printErrorMessage :: String -> Char -> IO ()
printErrorMessage [prev] c = do
  let row = state_matrix!!stateTransition 0 prev
  putStr $ "In expression \ESC[33m" ++ [prev] ++ [c] ++ "\ESC[0m\n\tExpected any of: "
  printRow row 0 False False
  putStr "\n\tFound: \ESC[33m"
  putStr [c]
  putStrLn "\ESC[0m"
printErrorMessage partial@(t:rest) c = do
  let row = state_matrix!!backtrackStates (reverse rest) t
  putStr $ "In expression \ESC[33m" ++ partial ++ [c] ++ "\ESC[0m\n\tExpected any of: "
  printRow row 0 False False
  putStr "\n\tFound: \ESC[33m"
  putStr [c]
  putStrLn "\ESC[0m"

-- Reserved word checking and special cases where state number doesn't match the C enum value
tokenStateToClass :: String -> Int -> Int
tokenStateToClass tok st | tok `elem` reserved_words = -1 * (fromJust (elemIndex tok reserved_words) + 1)
                         | st == 18                  = 10 -- Combine * and / to MOP
                         | st == 23                  = 21 -- Combine binary +/- to ADDOP
                         |  st == 15 
                         || st == 19 
                         || st == 35 
                         || st == 37
                         || st == 40                 = 13 -- Combine relational operators
                         | otherwise                 = st

-- Alphabet building, currently some parts are unused
symbols = "+-*/{}()=!<>,"
digits = "1234567890"
letters = "zxcvbnmasdfghjklqwertyuiopASDFGHJKLZXCVBNMQWERTYUIOP"
whitespace = "\n\t "
end_file = '\255' -- C's EOF macro is integer -1, signed 4-byte. 
                  -- Casting to unsigned byte (a la the char type) results in 255.
alphabet = symbols ++ digits ++ letters ++ whitespace ++ [end_file]

-- Structures and subroutines requiring modification later as I add more to the language
reserved_words = ["CLASS", "VAR", "CONST", "IF", "THEN", "PROCEDURE", "WHILE", "DO", "CALL", "ODD", "PRINT", "GET", "ARRAY"]
-- Note: the end states are just rows of their number because the value doesn't matter, we never check that state
--                 L,  D,  *,  /,  =,  <, ws, er,  ;,  +,  -,  {,  },  ,,  (,  ),  >,  !,  [,  ]
state_matrix = [[  5,  3,  2,  7, 11, 14,  0,  1, 17, 20, 22, 24, 26, 28, 30, 32, 34, 38, 41, 43 ], -- Start state
                [  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1 ], -- Error
                [ 18, 18, 18, 18, 18, 18, 18,  1,  1, 18, 18,  1,  1, 18, 18, 18,  1,  1,  1,  1 ], -- Intermediate * char
                [  1,  3,  4,  4,  4,  4,  4,  1,  4,  4,  4,  4,  1,  4,  1,  4,  4,  4,  1,  4 ], -- Intermediate digit
                [  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4 ], -- Integer
                [  5,  5,  6,  6,  6,  6,  6,  1,  6,  6,  6,  6,  1,  6,  6,  6,  6,  6,  6,  6 ], -- Intermediate letters/digits
                [  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6 ], -- Identifier
                [ 10, 10,  8, 10, 10, 10, 10,  1,  1, 10, 10,  1,  1,  1, 10, 10,  1,  1,  1, 10 ], -- Intermediate / char
                [  8,  8,  9,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8 ], -- /* and intermediate comment
                [  8,  8,  8,  0,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8 ], --  */ and intermediate comment
                [ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 ], -- Division operator
                [ 12, 12,  1,  1, 13,  1, 12,  1,  1,  1,  1,  1,  1,  1, 12,  1,  1,  1,  1,  1 ], -- Intermediate =
                [ 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12 ], -- Assignment operator
                [ 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13 ], -- Comparison operator
                [ 15, 15,  1,  1, 16, 15, 15,  1,  1, 15, 15,  1,  1,  1, 15,  1,  1,  1,  1,  1 ], -- Intermediate < char
                [ 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15 ], -- < operator 
                [ 19, 19,  1,  1,  1,  1, 19,  1,  1, 19, 19,  1,  1,  1, 19,  1,  1,  1,  1,  1 ], -- Intermediate for <=
                [ 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17 ], -- semicolon
                [ 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18 ], -- Multiplication
                [ 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19 ], -- <= operator   
                [ 21, 21,  1,  1,  1,  1, 21,  1,  1,  1,  1,  1,  1,  1, 21,  1,  1,  1,  1,  1 ], -- Intermediate +
                [ 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21 ], -- Addition operator
                [ 23, 23,  1,  1,  1,  1, 23,  1,  1,  1, 23,  1,  1, 23, 23,  1,  1,  1,  1,  1 ], -- Intermediate -
                [ 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23 ], -- Subtraction operator
                [ 25, 25,  1,  1,  1,  1, 25,  1,  1, 25, 25, 25, 25, 25, 25,  1,  1,  1,  1,  1 ], -- Intermediate { char
                [ 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25 ], -- Open block
                [ 27, 27,  1,  1,  1,  1, 27,  1, 27, 27, 27, 27, 27, 27,  1, 27,  1,  1,  1,  1 ], -- Intemediate } char
                [ 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27 ], -- Close block
                [ 29, 29,  1,  1,  1,  1, 29,  1,  1, 29, 29, 29, 29,  1, 29,  1,  1,  1,  1,  1 ], -- Intermediate , char
                [ 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29 ], -- Comma separator
                [ 31, 31,  1,  1,  1,  1, 31,  1,  1, 31, 31, 31, 31,  1, 31, 31,  1,  1,  1,  1 ], -- Intermediate (
                [ 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31 ], -- Left paren
                [ 33, 33,  1,  1,  1,  1, 33,  1, 33, 33, 33, 33, 33,  1, 33, 33,  1,  1,  1, 33 ], -- Intermediate )
                [ 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33 ], -- Right paren
                [ 35, 35,  1,  1, 36,  1, 35,  1,  1, 35, 35,  1,  1,  1, 35,  1,  1,  1,  1,  1 ], -- Intermediate >
                [ 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35 ], -- > operator
                [ 37, 37,  1,  1,  1,  1, 37,  1,  1, 37, 37, 37,  1,  1, 37,  1,  1,  1,  1,  1 ], -- Intermediate >=
                [ 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37 ], -- >= operator
                [  1,  1,  1,  1, 39,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1 ], -- Intermediate !
                [ 40, 40,  1,  1,  1,  1, 40,  1,  1, 40, 40,  1,  1,  1, 40,  1,  1,  1,  1,  1 ], -- Intermediate !=
                [ 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40 ], -- !=
                [ 42, 42,  1,  1,  1,  1, 42,  1,  1,  1,  1,  1,  1,  1, 42,  1,  1,  1,  1,  1 ], -- Intermediate [
                [ 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42 ], -- Left subscript
                [  1,  1, 44, 44, 44, 44, 44,  1, 44, 44, 44,  1, 44, 44,  1, 44, 44, 44,  1, 44 ], -- Intermediate ]
                [ 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44 ]] -- Right subscript
                
end_states =    [ 1, 4, 6, 10, 12, 13, 15, 17, 18, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 40, 42, 44 ] -- Stop the parse on these states
dump_states =   [ 0, 8, 9 ] -- Flush the token builder list on these states
stateTransition :: Int -> Char -> Int -- Automated row-column lookup
stateTransition s c = state_matrix!!s!!getCharType c

-- Unchanging helper subroutines
isEnd :: Int -> Bool
isEnd x = x `elem` end_states
dumpToken :: Int -> Bool
dumpToken s = s `elem` dump_states

-- Main token scanner, scans character by character and returns the newest token
-- Function to check for valid grammar from an input automaton, a partial token, and a dump token func
-- Returns the next list of terminals denoting a token, its class, and the start token to resume parsing on
scan :: D_F_Automaton state term -> [term] -> IO term -> (state -> Bool) -> IO ([term], state, term)
scan (sigma, delta, s, f) token gt dump = do
  t <- gt --  Get a new terminal (character)
  --  Get the new state based on the current terminal
  let st = delta s t
  -- Check for valid end-of-token, otherwise recurse with new state and slightly longer partial token
  if f st then return (token, st, t)
  else do
    if dump st then do -- Flush the entire token so far for the next parse, ie whitespace, comments, etc
      scan (sigma, delta, st, f) [] gt dump 
    else do
      scan (sigma, delta, st, f) (token ++ [t]) gt dump


-- Loop calling DFA scanner and writing tokens to the intermediate file
-- While running this function my computer completely filled with bees.
-- I think they must have a hive inside the log, because it wouldn't
--  make sense for them to have a hive inside the recursion.
-- (they couldn't store any honey in the comb beyond depth 1)
logAndRecurse :: (String, Int, Char) -> IO Int 
logAndRecurse (token, st, ch) = do
  -- TODO Error checking based on current token and character
  if st == 1 then do
    putStrLn "\ESC[31mLexicalli: ERROR\ESC[0m"
    printErrorMessage token ch
    return 1
  else do
    -- Allocate a CString called tok_string on the stack and marshal token into it,
    --  then log the token and its token class to the file
    withCString token (\tok_string -> 
      put_token tok_string ((CInt . fromIntegral) (tokenStateToClass token st)))
  
    -- Continue analysis
    if ch == end_file then do
      putStrLn "Lexicalli: Token Generation Success" -- file ended and we're in a successful state
      return 0
    else do
      -- Resume the parse in the state from the character
      -- The reason for this conditional is that I don't want a leading whitespace
      --  in my next token
      if ch `elem` whitespace then do
        tok <- scan (alphabet, stateTransition, 0, isEnd) [] getCharCast dumpToken
        logAndRecurse tok
      else do
        tok <- scan (alphabet, stateTransition, stateTransition 0 ch, isEnd) [ch] getCharCast dumpToken
        logAndRecurse tok
  
-- Called by C
run_scanner :: IO CInt
run_scanner = do
  putStrLn "Lexicalli: Starting token generation"
  
  -- Get the first token
  tok1 <- scan (alphabet, stateTransition, 0, isEnd) [] getCharCast dumpToken
  
  -- Continue the token scan
  ret <- logAndRecurse tok1
  return $ fromIntegral ret

-- Tell the compiler to generate C headers called "stubs" to interface
-- The linker will take those stubs and the object files from compiled Haskell code and package them together
--  for the final app
foreign export ccall run_scanner :: IO CInt