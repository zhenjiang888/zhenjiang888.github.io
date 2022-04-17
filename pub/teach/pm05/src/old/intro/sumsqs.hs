-- Exercise at class

-- Given three numbers, compute the sum of
--   squares of two bigger numbers.

sumsqs a b c = if a>=b then 
                  if b>=c then a^2+b^2
                  else a^2+c^2
	      else 
	          if a>=c then b^2+a^2
                  else b^2+c^2
          