-- DFA.hs
-- Generic Deterministic Finite State Automaton

module DFA where
  -- Main DFA structure  
  -- Generic type holding Sigma, delta, q_0, and F for a DFSA, needs types for its nonterminals and terminals
  -- Called T, M, S, Z in the notes, I used the mathematical terms because I built this
  --  before we covered it in class
  -- Deviations from mathematics: Doesn't require a state alphabet as 
  --  the state type is a generic parameter, so Q/K is removed from the tuple
  type D_F_Automaton state term = ([term], state->term->state, state, state->Bool)
