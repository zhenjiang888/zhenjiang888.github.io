-- 数を言葉に

-- 1-9の数字を英語の言葉に

conv1 :: Int -> String
conv1 x = unit !! (x-1)

unit = ["one", "two", "three", "four", "five",
	"six", "seven", "eight", "nine"]

-- 1-99の数字を対象に

conv2 x = combine2 (digit2 x)

digit2 x = (x `div` 10, x `mod` 10)

combine2 (0,y) = conv1 y
combine2 (1,y) = teens !! y
combine2 (x,0) = tens !! (x-2)
combine2 (x,y) = tens !! (x-2) ++ "-" ++ conv1 y

teens = ["ten", "eleven", "twelve", "thirteen", 
	 "fourteen", "fifteen", "sixteen",
	 "seventeen", "eighteen", "nineteen"]

tens = ["twenty", "thirty", "forty", "fifty",
	"sixty", "seventy", "eighty", "ninety"]

-- 1-999の数字を対象に

conv3 x = combine3 (digit3 x)

digit3 x = (x `div` 100, x `mod` 100)

combine3 (0,y) = conv2 y
combine3 (x,0) = conv1 x ++ " hundred"
combine3 (x,y) = conv1 x ++ " hundred and " 
		 ++ conv2 y

-- 1-999,999の数字を対象に

conv6 x = combine6 (digit6 x)

digit6 x = (x `div` 1000, x `mod` 1000)

combine6 (0,y) = conv3 y
combine6 (x,0) = conv3 x ++ " thousand"
combine6 (x,y) = conv3 x ++ " thousand" ++ lnk y
		 ++ conv3 y

lnk y = if y < 100 then " and " else " "

-- lnk y | y < 100 = " and "
--       | otherwise = " "

-- カレンダーの印刷

type Picture = [[Char]]  -- [String]

pic :: Picture
pic = [['1','2','3','4'],
       ['5','6','7','8']]

pic2str :: Picture -> String
pic2str [] = []
pic2str (l:p) = l ++ "\n" ++ pic2str p

display :: Picture -> IO ()
display p = putStr (pic2str p)

height :: Picture -> Int
height p = length p

width :: Picture -> Int
width p = length (head p)

-- 図形の構成

above :: Picture -> Picture -> Picture
p `above` q = p ++ q

beside :: Picture -> Picture -> Picture
p `beside` q = zipWith (++) p q

-- stack [] = []
-- stack (p:ps) = p `above` stack ps 

stack ps = foldr1 above ps
spread ps = foldr1 beside ps

empty (h,w) = copy (copy ' ' w) h

copy :: a -> Int -> [a]
copy x n = [ x | i <- [1..n]] 

block :: Int -> [Picture] -> Picture
block n ps = stack (map spread (group n ps))

group n xs = [take n (drop j xs) 
	      | j <- [0,n..length xs - n ]]

blockT n ps = spread (map stack (group n ps))

-- 図形の埋め込み

lframe (m,n) p = (p `beside` empty (h,n-w)) `above`
		 empty (m-h,n)
  where h = height p
	w = width p

-----------------------------------

-- カレンダーの構成

month_pic (mn,yr,fd,ml) = title mn yr `above` table fd ml

title mn yr = lframe (2,25) [mn ++ " " ++ show yr]

table fd ml = lframe (8,25) (daynames `beside` entries fd ml)

daynames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

entries fd ml = blockT 7 (dates fd ml)
dates fd ml = [date ml d | d <- [1-fd..42-fd]]
date ml d | d<1 || d>ml = [rjustify 3 " "]
	  | otherwise = [rjustify 3 (show d)]

rjustify n x = copy ' ' (n-length x) ++ x

-- top-level プログラム

calendar :: Int -> Picture
calendar = block 3 . map month_pic . months

months yr = zip4 mnames (copy yr 12) (fstdays yr) (mlengths yr)
  where 
  zip4 [] [] [] [] = []
  zip4 (x:xs) (y:ys) (z:zs) (w:ws) = (x,y,z,w) : zip4 xs ys zs ws

mnames = ["JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY",
          "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"]

mlengths yr = [31,feb,31,30,31,30,31,31,30,31,30,31]
	where feb | leap yr   = 29
                  | otherwise = 28
	      leap yr | yr `mod` 100 == 0 = yr `mod` 400 == 0
		      | otherwise         = yr `mod` 4 == 0

fstdays yr = take 12 (scanl oplus (jan1 yr) (mlengths yr))
  where s `oplus` m = (s+m) `mod` 7

jan1 yr = (yr + leapyr yr) `mod` 7
leapyr yr = yr `div` 4 - yr `div` 100 + yr `div` 400
