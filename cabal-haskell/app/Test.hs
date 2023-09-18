-- Test.hs
-- Dummy tests of my automaton implementations

import DFA
import PDA
import Stack

-- State and alphabet lists
states = ["off", "on", "done", "errored"]
alphabet = ['0', '1', '2']

-- Sample DFSA with 0s and 1s, 2 is the end-of-line character
-- state : String; token : Char;

-- DFSA Grammar specification
-- Modifies state according to passed state and token
-- (in programming this would be according to syntax and lexemes)
transition :: String -> Char -> String
transition s t | (t == alphabet!!0) = states!!0
               | (t == alphabet!!1) = states!!1
               | (t == alphabet!!2) = states!!2
               | otherwise          = states!!3

-- Checks for a valid end-of-list state
-- (in programming the token to trigger an eol state would be a semicolon)
end :: String -> Bool
end s | (s == states!!2) = True
      | otherwise = False

cond = validate (states, alphabet, transition, states!!1, end) "0010001011102"
-- Really cool note: a String is a [Char] so this works for this specific token type
-- If I used any other type I would supply a regularly defined list of that type
-- ['0', '1', '1', '0', '0', '2'] would also work for this function


-- PDA tests
s_els = ["disabled", "enabled", "ended", "invalidated"]
st :: Stack String
st = [] -- Initial stack

-- delta for PDA
-- Modifies state and stack based on current state and token
push_transition :: String -> Char -> (Stack String) -> (String, (Stack String))
push_transition s t stk | (s == states!!2)   = (states!!3, (push stk (s_els!!3)))
                        | (t == alphabet!!0) = (states!!0, (push stk (s_els!!0)))
                        | (t == alphabet!!1) = (states!!1, (push stk (s_els!!1)))
                        | (t == alphabet!!2) = (states!!2, (push stk (s_els!!2)))
                        | otherwise          = (states!!3, (push stk (s_els!!3)))
                  
-- F for PDA        
push_end :: String -> (Stack String) -> (Bool, (Stack String))
push_end s top | (s == states!!2 && (peek top) == s_els!!2) = (True, top)
               | otherwise                                  = (False, top)

res = push_down (states, alphabet, s_els, push_transition, states!!0, st, push_end) "0112"

recurse :: (Stack String) -> IO ()
recurse [] = print (states!!2)
recurse s = do
  print (peek s)
  recurse(pop s)


messwithres :: (Bool, (Stack String)) -> IO ()
messwithres (x, y) = do
  if x then (print "True") else (print "False")
  recurse y
  
eo_states = ["S", "A", "Z", "E"]
eo_even = "02468"
eo_odd = "13579"
eo_alph = eo_even ++ eo_odd
eo_stf :: String -> Char -> String
eo_stf s t | (s == eo_states!!0 && t `elem` eo_odd) = eo_states!!1
           | (s == eo_states!!0 && t `elem` eo_even) = eo_states!!2
           | (s == eo_states!!1 && t `elem` eo_odd) = eo_states!!1
           | (s == eo_states!!1 && t `elem` eo_even) = eo_states!!2
           | (s == eo_states!!2 && t `elem` eo_even) = eo_states!!2
           | (s == eo_states!!2 && t `elem` eo_odd) = eo_states!!1
           | otherwise                            = eo_states!!3
eo_end :: String -> Bool
eo_end s | (s == eo_states!!2) = True
         | otherwise           = False
         
eo_cond = validate (eo_states, eo_alph, eo_stf, eo_states!!0, eo_end) "21112"

main :: IO ()

{--
xa :: Stack Integer
xa = push (push (push xa 5) 7) 3
main = do
  let x = peek xa
  print x
  let xs = pop xa
  print (peek xs)
--}
  
{--}
main = do
  putStrLn "DFSA: "
  if (cond) then (putStrLn "True") else (putStrLn "False")
  putStrLn ""
  putStrLn "PDA: "
  messwithres res
  putStrLn ""
  putStrLn "EO: "
  if(eo_cond) then (putStrLn "True") else (putStrLn "False")
--}
