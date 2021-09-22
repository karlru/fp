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
nullid2 xs = foldr (\ a, b => if a == 0 then b+1 else b ) 0 xs

nullid3 : List Int -> Int
nullid3 xs = sum (map (\ x => if x == 0 then 1 else 0) xs)

nullid4 : List Int -> Int
nullid4 xs = cast (length (filter (==0) xs))

nullid5 : List Int -> Int
nullid5 xs = cast (length [x | x <- xs, x == 0])

length' : List a -> Int
length' a = foldl (\ b, a => b + 1) 0 a

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

isEven : Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S n)) = isEven n

all' : (a -> Bool) -> List a -> Bool
all' f a = ?rhs_all'

-- all' isEven [1, 2, 3] == False
-- all' isEven [2, 4, 6] == True