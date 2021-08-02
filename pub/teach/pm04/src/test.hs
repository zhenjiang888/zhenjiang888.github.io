sqrt1 :: Float -> Float
sqrt1 x = until (satisfy x) (improve x) x
	 where
	 satisfy x y = abs (y^2 - x) < 0.0001
	 improve x y = (y + x/y) / 2

f x = until (>x) (+3) 0