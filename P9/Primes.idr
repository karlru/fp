module Primes

-- brute force prime finder

modulo : Int -> Int -> Int
modulo x y = 
    if x < y then 
        x 
    else 
        modulo (x - y) y

-- first value of true will output whether divides, false whether not
divides : Bool -> Int -> Int -> Bool
divides b x y = 
    if (modulo x y) == 0 then
        b
    else
        if b then False else True

isPrime : Int -> Bool
isPrime x = 
    if x < 2 then 
        False
    else 
        recIsPrime x [2..x]
    where recIsPrime : Int -> List Int -> Bool
          recIsPrime x []        = True
          recIsPrime x (y :: ys) = 
            if y < x then
                if divides False x y then
                    True
                else recIsPrime x ys
            else
                True
