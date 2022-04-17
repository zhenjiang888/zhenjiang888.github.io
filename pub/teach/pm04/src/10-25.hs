norm :: (Int, Int) -> (Int, Int)
norm (x,y) = 
  (sign x * u `div` gcd u (abs y), 
   abs y `div` gcd u (abs y))
  where u = abs x

sign :: Int -> Int
sign x | x > 0 = 1
       | x == 0 = 0
       | x < 0 = -1

radd (u,v) (x,y) = norm (u*y+v*x, v*y)
rsub (u,v) (x,y) = norm (u*y-v*x, v*y)
rmul (u,v) (x,y) = norm (u*x, v*y)
rdiv (u,v) (x,y) = norm (u*y, v*x)

showrat :: (Int, Int) -> String
showrat (x,y) =
    if v == 1 then show u
    else show u ++ "/" ++ show v
    where (u,v) = norm (x,y)