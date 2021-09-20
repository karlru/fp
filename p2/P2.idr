module P2

import Data.Monoid.Exponentiation

fst : (a, b) -> a
fst (x, y) = x

length : List a -> Nat
length []        = 0
length (x :: xs) = 1 + P2.length xs

infixr 7 ++
(++) : List a -> List a -> List a
[] ++ ys        = ys
(x :: xs) ++ ys = x :: rec
    where rec : List a
          rec = P2.(++) xs ys

infixl 9 !!
partial
(!!) : List a -> Int -> a
(x :: xs) !! 0 = x
(x :: xs) !! n = xs !! (n-1)

replicate : Nat -> a -> List a
replicate 0 x     = []
replicate (S k) x = x :: rec
    where rec : List a
          rec = replicate k x

take : Nat -> List a -> List a
take _ Nil           = []
take Z x             = []
take (S k) (x :: xs) = x :: take k xs

snd : (a, b) -> b
snd (x, y) = y

null : List a -> Bool
null [] = True
null x = False

sum : List Integer -> Integer
sum []        = 0
sum (x :: xs) = x + sum xs

drop : Nat -> List a -> List a
drop _ Nil = []
drop Z x   = x
drop (S k) (x :: xs) = drop k xs

reverse : List a -> List a
reverse []        = []
reverse (x :: xs) = P2.(++) (reverse xs) [x]

multPairs : List (Integer, Integer) -> List Integer
multPairs []        = []
multPairs ((x, y) :: xs) = (x * y) :: multPairs xs

esimesed : List (a, b) -> List a
esimesed []             = []
esimesed ((x, y) :: xs) = x :: esimesed xs

otsi : Integer -> List Integer -> Bool
otsi n []        = False
otsi n (x :: xs) = if n == x then True else otsi n xs

dropLast : List a -> List a
dropLast []  = []
dropLast [_] = []
dropLast (x :: y :: xs) = x :: dropLast (y :: xs)

lisa' : Int -> Char -> List Char -> List Char
lisa' (-1) x []           = []
lisa' i x []           = x :: lisa' (-1) x []
lisa' (-1) x (y :: ys) = y :: lisa' (-1) x ys
lisa' 0 x y            = x :: lisa' (-1) x y
lisa' i x (y :: ys)    = y :: lisa' (i - 1) x ys

lisa : Int -> Char -> String -> String
lisa i x y = 
    if i < 0 then
        pack (lisa' 0 x (unpack y))
    else
        pack (lisa' i x (unpack y))

arvuta : List (Double, Nat) -> Double -> Double
arvuta [] x         = 0
arvuta ((p, n) :: ps) x  = p * x^n + arvuta ps x
