module Text where

-- Since lines, words, unlines, unwords are functions in Prelude.hs
-- we use lines', words', unlines' and unwords' instead.

type Text' = [Char]

-- 4.3.1

type Line' = [Char]

lines' :: Text' -> [Line']
unlines' :: [Line'] -> Text'

unlines' = foldr1 oplus
	where xs `oplus` ys = xs ++ "\n" ++ ys

lines' = foldr otimes [[]]
	where x `otimes` xss | x=='\n'   = [[]] ++ xss
			     | otherwise = [[x] ++ head xss] ++ tail xss

-- 4.3.2

type Word' = [Char]

words' :: Line' -> [Word']
unwords' :: [Word'] -> Line'

unwords' = foldr1 oplus
	where xs `oplus` ys = xs ++ " " ++ ys

words' = filter (/=[]) . foldr otimes [[]]
	where x `otimes` xss | x==' '   = [[]] ++ xss
			     | otherwise = [[x] ++ head xss] ++ tail xss

-- 4.3.3

type Para = [Line']

paras :: [Line'] -> [Para]
unparas :: [Para] -> [Line']

unparas = foldr1 oplus
	where xs `oplus` ys = xs ++ [[]] ++ ys

paras = filter (/=[]) . foldr otimes [[]]
	where xs `otimes` xss | xs==[]    = [[]] ++ xss
		 	      | otherwise = [[xs] ++ head xss] ++ tail xss

-- 4.3.4

countlines = length . lines'
countwords = length . concat . map words' . lines'
countparas = length . paras . lines'

normalise :: Text' -> Text'
normalise = unparse . parse

parse :: Text' -> [[[Word']]]
parse = map (map words') . paras . lines'

unparse :: [[[Word']]] -> Text'
unparse = unlines' . unparas . map (map unwords')

--  4.3.5

fill :: Int -> [Word'] -> [[Word']]
fill m [] = []
fill m ws = [fstline] ++ fill m restwds 
	where fstline = take n ws
	      restwds = drop n ws
	      n = greedy m ws

greedy m ws = maximum [ length us | us <- inits ws, 
				    length (unwords' us) <= m ]

inits = tail . scanl oplus []
	where xs `oplus` x = xs ++ [x]

filltext m = unparse . map (fill m) . testparas
testparas = map linewords . paras  . lines'
linewords = concat . map words'

--- test

t =    "This     is    the   first line \n" 
    ++ "of    the  first paragraph.\n" 
    ++ "\n" 
    ++ "\n" 
    ++ "This is the    second  \n" 
    ++ "paragraph."

{-

Text> putStr t
This     is    the   first line
of    the  first paragraph.


This is the    second
paragraph.
(8 reductions, 15 cells)

Text> putStr (filltext 30 t)
This is the first line of the
first paragraph.

This is the second paragraph.
(6053 reductions, 9486 cells)

Text> putStr (filltext 20 t)
This is the first
line of the first
paragraph.

This is the second
paragraph.
(6618 reductions, 10298 cells)   

-}

   

