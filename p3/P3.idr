filter : (a -> Bool) -> List a -> List a
filter f []        = []
filter f (x :: xs) = 
    if f x then 
        x :: filter f xs 
    else 
        filter f xs

nullid1 : List Int -> Int
nullid1 []        = 0
nullid1 (x :: xs) = 
    if 0 == x then
        1 + nullid1 xs
    else
        nullid1 xs

nullid2 : List Int -> Int
nullid2 xs = foldr f 0 xs
    where f : Int -> Int -> Int
          f a b = if a == 0 then b+1 else b

nullid3 : List Int -> Int
nullid3 xs = ?rhs_nullid3

nullid4 : List Int -> Int
nullid4 xs = ?rhs_nullid4

nullid5 : List Int -> Int
nullid5 xs = ?rhs_nullid5

length' : List a -> Int
length' a = ?rhs_length'

productList : List Int -> Int
productList xs = foldr (\ a, b => a * b) 1 xs
{-
productList [3,2,0]
    0 * 1 ==> 0
    0 * 2 ==> 0
    0 * 3 ==> 0
-}

append' : List a -> List a -> List a
append' xs ys = ?rhs_append'

all' : (a -> Bool) -> List a -> Bool
all' f a = ?rhs_all'