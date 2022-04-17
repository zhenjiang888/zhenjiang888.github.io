-- p.36

roots :: (Float,Float,Float) -> (Float,Float)
roots (a,b,c) | d>=0  = (r1,r2)
   where r1 = (-b+r) / (2*a)
         r2 = (-b-r) / (2*a)
         r  = sqrt d
         d  = b^2 - 4*a*c

{--------------
 test examples
 --------------

Main> roots (1,2,1)
(-1.0,-1.0)
(133 reductions, 220 cells)
Main> roots (1,-7,12)
(4.0,3.0)
(133 reductions, 218 cells)
Main> roots (2,1,1)

Program error: {roots (2.0,1.0,1.0)}
(98 reductions, 226 cells)  

}