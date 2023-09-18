-- Stack.hs
-- Functional linked stack
-- Really just a wrapper for a list but much more intuitive to read than 
--  Haskell's list syntax when used in state transition functions

module Stack where
  type Stack d = [d]
  
  push :: Stack d -> d -> Stack d
  push s e = e:s -- Construct list from existing list 
  
  pop :: Stack d -> Stack d
  pop s = tail s
  
  peek :: Stack d -> d
  peek s = head s