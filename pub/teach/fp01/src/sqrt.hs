-- p.24

-- Note that sqrt is a functionn in Prelude.hs, so we choose a 
-- different name: sqrt'.

sqrt' x = until (satis x) (improve x) x
  where 
      improve x y = (y + x/y) / 2
      satis x y = abs (y^2 - x) < eps
      eps = 0.0001

{------------
 test example 
 ------------

Main> sqrt 4
2.0
(16 reductions, 20 cells)

Main> sqrt 5.5
2.34521
(15 reductions, 23 cells)

Main> sqrt 0
0.0
(16 reductions, 20 cells)

Main> sqrt (-3)
Program error: {primSqrtDouble (-3.0)}
(12 reductions, 59 cells)                

-}

