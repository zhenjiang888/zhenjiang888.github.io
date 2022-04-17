norm (x,y) | y /= 0 = (s * (u `div` d), v `div` d)
  where u = abs x
        v = abs y
        d = gcd u v
	s = sign (x*y)
sign x | x>0  = 1
       | x==0 = 0
       | x<0  = -1

radd (x,y) (u,v) = norm (x*v+u*y,y*v)
rsub (x,y) (u,v) = norm (x*v-u*y,y*v)
rmul (x,y) (u,v) = norm (x*u,y*v)
rdiv (x,y) (u,v) = norm (x*v,y*u)

compare' op (x,y) (u,v) = op (x*v) (y*u)
requals  = compare' (==)
rless    = compare' (<)
rgreater = compare' (>)

showrat (x,y) = if v==1 then show u
                        else show u ++ "/" ++ show v
  where (u,v) = norm (x,y)

{---------------
  test examples
 ---------------

Main> norm (6,4)
(3,2)
(265 reductions, 385 cells)
Main> radd (6,4) (1,2)
(2,1)
(276 reductions, 391 cells)
Main> rsub (6,4) (1,2)
(1,1)
(276 reductions, 391 cells)
Main> rmul (6,4) (2,3)
(1,1)
(268 reductions, 377 cells)
Main> rdiv (6,4) (6,4)
(1,1)
(268 reductions, 377 cells)
Main> rless (18,23) (23,27)
True
(35 reductions, 60 cells)   

-}
