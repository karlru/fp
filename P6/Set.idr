module Set

export 
data Set a = MkSet (List a)

memList : Eq a => a -> List a -> Bool
memList x [] = False
memList x (y :: xs)  with (x==y)
    memList x (y :: xs) | True  = True
    memList x (y :: xs) | False = memList x xs

    
deleteList : Eq a => a -> List a -> List a
deleteList x [] = []
deleteList x (y :: xs)  with (x==y)
    deleteList x (y :: xs) | True  = xs
    deleteList x (y :: xs) | False = y :: deleteList x xs

export 
empty : Set a
empty = MkSet []

export 
single : a -> Set a
single x = MkSet [x]

export 
mem : Eq a => a -> Set a -> Bool
mem x (MkSet xs) = memList x xs

export 
add : Eq a => a -> Set a -> Set a
add x (MkSet xs) = if memList x xs then MkSet xs else MkSet (x::xs)

export 
delete : Eq a => a -> Set a -> Set a
delete x (MkSet xs) = MkSet (deleteList x xs)

infix 7 \\
export 
(\\) : Eq a => Set a -> Set a -> Set a
(MkSet xs) \\ (MkSet ys) = MkSet (foldr deleteList xs ys)

export 
Cast a b => Cast (Set a) (List b) where
    cast (MkSet xs) = map cast xs

export 
setToList : Set a -> List a 
setToList (MkSet xs) = xs

export
foldr : (a -> b -> b) -> b -> Set a -> b 
foldr f e (MkSet []) = e
foldr f e (MkSet (x :: xs)) = f x (foldr f e xs)

export
foldl : (b -> a -> b) -> b -> Set a -> b 
foldl f e (MkSet []) = e
foldl f e (MkSet (x :: xs)) = foldl f (f e x) xs

export 
listToSet : Eq a => List a -> Set a
listToSet [] = empty
listToSet (x :: xs) = add x (listToSet xs)

export 
union : Eq a => Set a -> Set a -> Set a
union xs ys = foldl (flip add) xs (setToList ys)

export
size : Set a -> Int
size (MkSet xs) = foldl (\ x, _ => x+1) 0 xs

export
Eq a => Eq (Set a) where
    a == b = size a == size b && foldl (\ x,y => x && mem y b) True a 

