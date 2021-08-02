module Convert where

-- constant table

units = [ "one", "two", "three", "four", "five",
          "six", "seven", "eight", "nine"]

teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", 
         "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens = ["twenty", "thirty", "forty", "fifty", "sixty",
        "seventy", "eighty", "ninety"]

-- convert numbers less than 100

convert2 n = combine2 (digits2 n)

digits2 n = (n `div` 10, n `mod` 10)

combine2 (0,u+1) = units !! u
combine2 (1,u) = teens !! u
combine2 (t+2,0) = tens !! t
combine2 (t+2,u+1) = tens !! t ++ "-" ++ units !! u

-- convert numbers less than 1000

convert3 n = combine3 (digits3 n)

digits3 n = (n `div` 100, n `mod` 100)

combine3 (0,t+1) = convert2 (t+1)
combine3 (h+1,0) = units !! h ++ " hundred"
combine3 (h+1,t+1) = units !! h ++ " hundred and " ++ convert2 (t+1)

-- convert numbers less than 1000,000

convert6 n = combine6 (digits6 n)

digits6 n = (n `div` 1000, n `mod` 1000)

combine6 (0,h+1) = convert3 (h+1)
combine6 (m+1,0) = convert3 (m+1) ++ " thousand"
combine6 (m+1,h+1) = convert3 (m+1) ++ " thousand" 
                     	++ link (h+1) ++ convert3 (h+1)

link h | h < 100 = " and "
       | otherwise = " "

{------
  test
 ------

Convert> convert6 308000
"three hundred and eight thousand"
(985 reductions, 1350 cells)

Convert> convert6 369027
"three hundred and sixty-nine thousand and twenty-seven"
(1837 reductions, 2547 cells)

Convert> convert6 369401
"three hundred and sixty-nine thousand four hundred and one"
(1851 reductions, 2548 cells)      

-}