capitalise x 
  | isLower x 	= chr (offset + ord x)
  | otherwise   = x
 where offset = ord 'A' - ord 'a'

{---------------
  test examples
 ---------------

Main> capitalise 'd'
'D'
(56 reductions, 73 cells)
Main> capitalise 'z'
'Z'
(56 reductions, 73 cells)
Main> capitalise 'S'
'S'
(39 reductions, 59 cells)     

-}
