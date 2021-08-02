module Arithmetic where

-- variable length integer

type VInt = [Bigit]
type Bigit = Int
b = 10 :: Int

-- drop unnecessary front zeros
--  strep [0,0,1,2] = [1,2]

strep :: VInt -> VInt
strep xs | ys == []  = [0]
         | otherwise = ys
    where ys = dropWhile (==0) xs

-- alignment
--  align [1,2,3] [4] = ([1,2,3],[0,0,4])

align :: VInt -> VInt -> (VInt,VInt)
align xs ys | n > 0     = (copy 0 n ++ xs, ys)
            | otherwise = (xs, copy 0 (-n) ++ ys)
    where n = length ys - length xs
          copy x n = [x | i<-[1..n]] 

-------------
-- comparison
-------------

vcompare :: (VInt->VInt->Bool) -> VInt -> VInt -> Bool
vcompare op xs ys = op us vs
    where (us,vs) = align xs ys

veq = vcompare (==)
vleq = vcompare (<=)
vless = vcompare (<)

-- normalization
--  norm [3,-3,-2] = [2,6,8]
-- 	note: -3 `mod` 10 = 7; -3 `div` 10 = -1
--  norm [-10,-1] = [-1,8,9,9]
--  norm [987] = [9,8,7]

norm :: VInt -> VInt
norm = strep . addCarry . foldr carry [0]
  where carry :: Bigit -> VInt -> VInt
	carry x (c:xs) = (x+c) `div` b : (x+c) `mod` b : xs

addCarry :: VInt -> VInt
addCarry (c:xs) = if (-1 <= c) && (c < b) then c : xs
                  else addCarry (c `div` b : c `mod` b : xs)

-------------------------------------
-- addition
--  vadd [7,3,7] [4,6,9] = [1,2,0,6]
-------------------------------------
vadd :: VInt -> VInt -> VInt
vadd xs ys = norm (zipWith (+) us vs)
    where (us,vs) = align xs ys

--------------------------------------
-- substraction
--  vsub [1,0,6] [3,7,5] = [-1,7,3,1]
--------------------------------------

vsub :: VInt -> VInt -> VInt
vsub xs ys = norm (zipWith (-) us vs)
    where (us,vs) = align xs ys

--------------------------------------
-- negation
-- vnegate [-1,7,3,1] = [2,6,9]
--------------------------------------

negative xs = head xs < 0
vnegate = norm . map neg
neg x = -x

--------------------------------------
-- multiplication
--  vmul [1,2,3] [4,5] = [5,5,3,5]
--------------------------------------

vmul xs ys = foldl1 oplus (psums xs ys)
    where psums xs ys = [norm (map (y*) xs) | y<-ys]
          xs `oplus` ys = vadd (xs++[0]) ys

--------------------------------------
-- div and mod
--  vqrm [1,7,8,4] [6,2] = ([2,8],[4,8])
--  bqrm [1,7,8,4] 6 = ([2,9,7],2)
--------------------------------------

-- divalg [1,7,8,4] [6,2] = [(0,[1]),(0,[1,7]),(2,[5,4]),(8,[4,8])]

divalg xs ys = scanl (dstep ys) (0,take m xs) (drop m xs)
    where m = length ys - 1

dstep ys (q,rs) x | length xs < length ys      = astep xs ys
                  | length xs == length ys     = bstep xs ys
                  | length xs == length ys + 1 = cstep xs ys
	where xs = rs ++ [x]

astep xs ys = (0,xs)

bstep xs ys | negative zs = (0,xs)
            | otherwise   = (1,zs) -- note: head ys >= b `div` 2 
	where zs = vsub xs ys

cstep xs ys | vless rs0 ys = (q,rs0)
            | vless rs1 ys = (q+1,rs1)
            | otherwise    = (q+2,rs2)
	where rs0 = vsub xs (bmul ys q)
	      rs1 = vsub rs0 ys
	      rs2 = vsub rs1 ys
	      q = guess xs ys - 2

guess (x0:x1:xs) (y1:ys) | x0>=y1    = b-1
                         | otherwise = (x0*b+x1) `div` y1

-- 
vqrm xs ys = (strep qs, strep rs)
	where qs = map fst ds
	      rs = bdiv (snd (last ds)) d
	      ds = divalg (bmul xs d) (bmul ys d)
	      d  = b `div` (head ys + 1)

-- 
bqrm [x] d = ([x `div` d], x `mod` d)
bqrm (x:xs) d = (strep qs, last rs `mod` d)
	where qs = map (`div` d) rs
	      rs = scanl oplus x xs
	      r `oplus` x = b * (r `mod` d) + x
		-- r(i+1) = b * (r(i) `mod` d) + x(i+1)

bdiv xs d = fst (bqrm xs d)
bmod xs d = snd (bqrm xs d)
bmul xs d = vmul xs [d]

v1 = [7,3,7] :: [Int]
v2 = [4,6,9] :: [Int]


