leap1 y = y `mod` 4 == 0  &&
          (y `mod` 100 /= 0 || y `mod` 400 == 0)

leap2 y | y `mod` 100 == 0   = y `mod` 400 == 0
        | otherwise          = y `mod` 4 == 0

{------------
 test example 
 ------------

Main> leap1 0
True
(203 reductions, 286 cells)
Main> leap1 4
True
(151 reductions, 222 cells)
Main> leap1 7
False
(89 reductions, 138 cells)
Main> leap1 2000
True
(203 reductions, 294 cells)
Main> leap1 2001
False
(89 reductions, 138 cells)         

-}