sqrt1 x = until (satisfy x) (improve x) x
  where 
      improve x y = (y + x/y) / 2
      satisfy x y = abs (y^2 - x) < eps
      eps = 0.0001

