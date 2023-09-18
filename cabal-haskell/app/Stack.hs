-- Functional linked stack

module Stack where
  type Stack d = [d]
  
  push :: Stack d -> d -> Stack d
  push s e = e:s -- Construct list from existing list 
  
  pop :: Stack d -> Stack d
  pop s = tail s
  
  peek :: Stack d -> d
  peek s = head s