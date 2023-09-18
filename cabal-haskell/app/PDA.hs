module PDA where
  import Stack
  -- Defines a Pushdown Automaton with Q, Sigma, Gamma, delta, q_0, Z, and F
  -- Deviations from formula: 
  --  Z is a stack structure instead of just the initial stack element
  --  push_down returns a tuple of the validity and the stack for viewing after completion
  type P_D_Automaton state token stack_el = ([state], 
                                             [token], 
                                             [stack_el], 
                                             (state->token->(Stack stack_el)->(state, (Stack stack_el))), 
                                             state, 
                                             (Stack stack_el), 
                                             (state->(Stack stack_el)->(Bool, (Stack stack_el))))
  -- Function to input the token list to the state transition function
  push_down :: P_D_Automaton state token stack_el -> [token] -> (Bool, (Stack stack_el))
  push_down (q, sigma, gamma, delta, s, stack, f) input =
    (uncurry f) (next_token s stack input)
    -- Send current state, stack var, and next token to delta which modifies states and stack
    where next_token cs stk [] = (cs, stk)
          next_token cs stk (tok:rest) = (uncurry next_token) (delta cs tok stk) rest
