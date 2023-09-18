-- DFA.hs
-- Generic Deterministic Finite State Automaton

module DFA where
  -- Generic type holding Q, Sigma, delta, q_0, and F for a DFSA
  -- Called K, T, M, S, Z in the notes, I used the mathematical terms because I built this
  --  before we covered it in class
  type D_F_Automaton state token = ([state], [token], (state->token->state), state, (state->Bool))
  -- Function to check for valid grammar from an input automaton and string
  validate :: D_F_Automaton state token -> [token] -> Bool
  validate (q, sigma, delta, s, f) input =
    f (next_token s input)
    -- Send current state and head token to delta with the rest of the string as input
    -- Left recursive, may need fix later for assignment? maybe not, idk if ast can be used with fsms
    where next_token cs [] = cs
          next_token cs (tok:rest) = next_token (delta cs tok) rest
