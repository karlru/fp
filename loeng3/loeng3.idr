
-- lambdad

-- (\ x => x) "a"
-- (\ x, y, z => x+y+z) 1 2 3


-- map

%hide map 

map : (a -> b) -> List a -> List b
map f []        = []
map f (x :: xs) = f x :: map f xs

-- map f [x1, x2, ..., xn] = [f x1, f x2, ..., f xn]


inverses: List Double -> List  Double
inverses xs = map (\ x => 1/x) xs

%hide foldr

foldr : (a -> b -> b) -> b -> List a -> b
foldr f y []        = y
foldr f y (x :: xs) = f x (foldr f y xs) 

-- foldr f a [x1, x2, ..., xn] = x1 `f` (x2 `f` (... `f` (xn `f` a))) 

map' : (a -> b) -> List a -> List b
map' f = foldr g []
   where g : a -> List b -> List b
         g x zs = f x :: zs

-- foldr g [] [x1, x2, ..., xn] 
--   == x1 `g` (x2 `g` (... `g` (xn `g` []))) 
--   == f x1 :: (f x2 :: (... :: (f xn :: []))) 

takeWhile : (a -> Bool) -> List a -> List a
takeWhile p = foldr g []
   where g : a -> List a -> List a
         g x zs = if p x then x :: zs else []

-- takeWhile (<10) [2..100]

%hide foldl

foldl : (b -> a -> b) -> b -> List a -> b
foldl f y []        = y
foldl f y (x :: xs) = foldl f (f y x) xs

-- foldl f a [x1, x2, ..., xn] = (((a `f` x1) `f` x2) `f` ...) `f` xn

%hide reverse

reverse : List a -> List a
reverse = foldl g []
   where g : List a -> a -> List a
         g xs x = x :: xs

-- reverse [x1, x2, ...,xn]
--   == foldl g [] [x1, x2, ...,xn]
--   == ((([] `g` x1) `g` x2) `g` ...) `g` xn
--   == xn :: ((([] `g` x1) `g` x2) `g` ...)
--   == xn :: (... :: (x2 :: ([] `g` x1)) 
--   == xn :: (... :: (x2 :: (x1 :: [])))
