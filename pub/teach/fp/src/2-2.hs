type Assoc a b = a -> b 

none :: Assoc a b 
none x = undefined 

lookup' :: Assoc a b -> a -> b 
lookup' h x = h x 

update :: Eq a => Assoc a b -> a -> b -> Assoc a b 
update h x v y 
    | x==y        = v
    | otherwise   = lookup' h y 


