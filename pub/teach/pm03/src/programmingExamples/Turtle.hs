module Turtle where

type State = (Direction,Pen,Point)
type Direction = Int  -- 0:North, 1:East, 2:South | West
type Pen = Bool       
type Point = (Int,Int)

type Command = State -> State

--  W  -----y-----> E
--  N  -----x-----> S
move :: Command                    
move (0,p,(x,y)) = (0,p,(x-1,y))   
move (1,p,(x,y)) = (1,p,(x,y+1))
move (2,p,(x,y)) = (2,p,(x+1,y))
move (3,p,(x,y)) = (3,p,(x,y-1))

right,left :: Command
right (d,p,(x,y)) = ((d+1) `mod` 4,p,(x,y))
left (d,p,(x,y)) = ((d-1) `mod` 4,p,(x,y))

up,down :: Command
up (d,p,(x,y)) = (d,False,(x,y))
down (d,p,(x,y)) = (d,True,(x,y))

square k = [down] ++ concat (copy side 4) ++ [up]
	where side = copy move k ++ [right]

copy x n = [x | j<-[1..n]]

display :: [Command] -> [Char]
display = layout . picture . trail . turtle

turtle :: [Command] -> [State]
turtle = scanl applyto (0,False,(0,0))
	where applyto x f = f x

trail :: [State] -> [Point]
trail ss = [(x,y) | (_,p,(x,y)) <- ss, p]

picture :: [Point] -> [[Char]]
picture = symbolise . bitmap
	where symbolise = map (map mark)
	      mark True = '*'
	      mark False = ' '

bitmap ps = [[(x,y) `elem` ps | y<-yran] | x<-xran]
	where xran = range' (map fst ps)
	      yran = range' (map snd ps)
range' xs = range (minimum xs, maximum xs)

layout = foldr oplus []
	where x `oplus` r = x ++ "\n" ++ r

{- test

Turtle> putStr (display (square 4))
*****
*   *
*   *
*   *
*****
 :: IO ()
(4513 reductions, 6465 cells)     

-}



