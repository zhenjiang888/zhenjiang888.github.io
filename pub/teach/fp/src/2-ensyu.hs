type Assoc a b = [(a,b)]

none :: Assoc a b   
none = []

update :: Assoc a b -> a -> b -> Assoc a b
update h x v = (x,v) : h

lookup1 :: Eq a => Assoc a b -> a -> b
lookup1 [] x = undefined
lookup1 ((x',v'):h') x 
      | x==x'     = v'
      | otherwise = lookup1 h' x
