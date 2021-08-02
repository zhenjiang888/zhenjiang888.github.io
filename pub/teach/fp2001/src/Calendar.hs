module Calendar where

-- 4.5.1

type Picture = [[Char]]

height,width :: Picture -> Int
height p = length p
width p = length (head p)

above,beside :: Picture -> Picture -> Picture
p `above` q | width p == width q = p++q
p `beside` q | height p == height q = zipWith (++) p q

stack,spread :: [Picture] -> Picture
stack = foldr1 above
spread = foldr1 beside

empty :: (Int,Int) -> Picture
empty (h,w) = copy (copy ' ' w) h

block :: Int -> [Picture] -> Picture
block n = stack . map spread . group n
group n xs = [take n (drop j xs) | j <- [0,n..(length xs-n)]]

blockT :: Int -> [Picture] -> Picture
blockT n = spread . map stack . group n

lframe :: (Int,Int) -> Picture -> Picture
lframe (m,n) p = (p `beside` empty (h,n-w)) `above` empty (m-h,n)
	where h = height p
	      w = width p

display = unlines

calendar :: Int -> String
calendar = display . block 3 . map picture . months

-- 4.5.2

picture (mn,yr,fd,ml) = title mn yr `above` table fd ml

title mn yr = lframe (2,25) [mn ++ " " ++ show yr]

table fd ml = lframe (8,25) (daynames `beside` entries fd ml)
daynames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

entries fd ml = blockT 7 (dates fd ml)
dates fd ml = map (date ml) [(1-fd)..(42-fd)]
date ml d | d<1 || ml < d = [rjustify 3 " "]
          | otherwise     = [rjustify 3 (show d)]

-- 4.5.3

months yr = zip4 mnames (copy yr 12) (fstdays yr) (mlengths yr)
  	where zip4 [] [] [] [] = []
	      zip4 (x:xs) (y:ys) (z:zs) (u:us) 
		= (x,y,z,u) : zip4 xs ys zs us

mnames = ["JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY",
          "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"]

mlengths yr = [31,feb,31,30,31,30,31,31,30,31,30,31]
	where feb | leap yr   = 29
                  | otherwise = 28
	      leap yr | yr `mod` 100 == 0 = yr `mod` 400 == 0
		      | otherwise         = yr `mod` 4 == 0

fstdays yr = take 12 (map (`mod` 7) (scanl (+) (jan1 yr) (mlengths yr)))
jan1 yr = (yr + (yr-1) `div` 4 - (yr-1) `div` 100 + (yr-1) `div` 400) `mod` 7

-- other utility functions

copy x n = [x | i<-[1..n]]

rjustify n x = space (n-m) ++ x
	where m = length x
	      space n = [' ' | i<-[1..n]]

{-

Calendar> putStr (calendar 2002)
JANUARY 2002             FEBRUARY 2002            MARCH 2002

Sun     6 13 20 27       Sun     3 10 17 24       Sun     3 10 17 24 31
Mon     7 14 21 28       Mon     4 11 18 25       Mon     4 11 18 25
Tue  1  8 15 22 29       Tue     5 12 19 26       Tue     5 12 19 26
Wed  2  9 16 23 30       Wed     6 13 20 27       Wed     6 13 20 27
Thu  3 10 17 24 31       Thu     7 14 21 28       Thu     7 14 21 28
Fri  4 11 18 25          Fri  1  8 15 22          Fri  1  8 15 22 29
Sat  5 12 19 26          Sat  2  9 16 23          Sat  2  9 16 23 30

APRIL 2002               MAY 2002                 JUNE 2002

Sun     7 14 21 28       Sun     5 12 19 26       Sun     2  9 16 23 30
Mon  1  8 15 22 29       Mon     6 13 20 27       Mon     3 10 17 24
Tue  2  9 16 23 30       Tue     7 14 21 28       Tue     4 11 18 25
Wed  3 10 17 24          Wed  1  8 15 22 29       Wed     5 12 19 26
Thu  4 11 18 25          Thu  2  9 16 23 30       Thu     6 13 20 27
Fri  5 12 19 26          Fri  3 10 17 24 31       Fri     7 14 21 28
Sat  6 13 20 27          Sat  4 11 18 25          Sat  1  8 15 22 29

JULY 2002                AUGUST 2002              SEPTEMBER 2002

Sun     7 14 21 28       Sun     4 11 18 25       Sun  1  8 15 22 29
Mon  1  8 15 22 29       Mon     5 12 19 26       Mon  2  9 16 23 30
Tue  2  9 16 23 30       Tue     6 13 20 27       Tue  3 10 17 24
Wed  3 10 17 24 31       Wed     7 14 21 28       Wed  4 11 18 25
Thu  4 11 18 25          Thu  1  8 15 22 29       Thu  5 12 19 26
Fri  5 12 19 26          Fri  2  9 16 23 30       Fri  6 13 20 27
Sat  6 13 20 27          Sat  3 10 17 24 31       Sat  7 14 21 28

OCTOBER 2002             NOVEMBER 2002            DECEMBER 2002

Sun     6 13 20 27       Sun     3 10 17 24       Sun  1  8 15 22 29
Mon     7 14 21 28       Mon     4 11 18 25       Mon  2  9 16 23 30
Tue  1  8 15 22 29       Tue     5 12 19 26       Tue  3 10 17 24 31
Wed  2  9 16 23 30       Wed     6 13 20 27       Wed  4 11 18 25
Thu  3 10 17 24 31       Thu     7 14 21 28       Thu  5 12 19 26
Fri  4 11 18 25          Fri  1  8 15 22 29       Fri  6 13 20 27
Sat  5 12 19 26          Sat  2  9 16 23 30       Sat  7 14 21 28
                                                                     
-}





