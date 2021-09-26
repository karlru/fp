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
    if x == 0 then
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
append' xs ys = foldr (\ x, b => x :: b) xs ys

isEven : Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S n)) = isEven n

all' : (a -> Bool) -> List a -> Bool
all' f a  = foldr (\ x, y => if f x == False then False else y) True a

reverse' : List a -> List a
reverse' xs = foldl rev df xs
  where
    df : List a
    df = []
    rev : List a -> a -> List a
    rev x y = y :: x

reverse2 : List a -> List a
reverse2 xs = foldl (\ a, b => b :: a) [] xs

eemaldaNullid : List Int -> List Int
eemaldaNullid xs = foldr rem df xs
  where
    df : List Int
    df = []
    rem : Int -> List Int -> List Int
    rem x y = if x == 0 then y else x :: y

eemaldaNullid2 : List Int -> List Int
eemaldaNullid2 xs = foldr (\ a, b => if a==0 then b else a :: b) [] xs

allEqual : List Int -> Bool
allEqual []        = True
allEqual (x :: xs) = x == valChange x xs || False
    where
        valChange : Int -> List Int -> Int
        valChange x xs = foldr (\ a, b => if a == x then b else a) x xs

unzip' : List (a, b) -> (List a, List b)
unzip' [] = ([], [])
unzip' xs = foldr f x xs
  where
    x : (List a, List b)
    x = ([], [])
    f : (a, b) -> (List a, List b) -> (List a, List b)
    f (a, b) (as, bs) = (a :: as, b :: bs)

removeAll1 : Int -> List Int -> List Int
removeAll1 n xs = foldr (\ x, y => if x == n then y else x :: y) [] xs

removeAll2 : Int -> List Int -> List Int
removeAll2 n xs = filter (\ x => if x==n then False else True) xs

removeAll3 : Int -> List Int -> List Int
removeAll3 n xs = [x | x <- xs, not x n]
    where
        not : Int -> Int -> Bool
        not x y = if x == y then False else True

any' : (a -> Bool) -> List a -> Bool
any' p xs = foldr (\ x, y => p x || y) False xs
