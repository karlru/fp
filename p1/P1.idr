sumInt : Int -> Int
sumInt 0 = 0
sumInt x = sumInt(x-1) + x

fib : Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib(x - 1) + fib(x - 2)

modulo : Int -> Int -> Int
modulo x y = 
    if x < y then 
        x 
    else 
        modulo (x - y) y

syt : Int -> Int -> Int
syt x 0 = x
syt x y = syt y (mod x y)

mc : Int -> Int
mc x = 
    if x > 100 then 
        x - 10 
    else 
        mc(mc(x + 11))

hanoi : Int -> Int
hanoi 1 = 1
hanoi x = 
    if x > 1 then 
        2 * hanoi(x - 1) + 1 
    else 
        -1

ack : Int -> Int -> Int
ack 0 n = n + 1
ack m n = 
    if n == 0 then 
        ack (m - 1) 1 
    else 
        ack (m - 1) (ack m (n - 1))

korda : Int -> (Int -> Int) -> Int -> Int
korda 0 f x = x
korda n f x = f (korda (n - 1) f x)
 
inc : Int -> Int
inc x = x + 1

-- add : Int -> Int -> Int
-- add x 0 = x
-- add x y = f x y

aste : Int -> Int -> Int
aste x 0 = 1
aste 0 n = 0
aste x n = x * aste x (n - 1)

-- p : Int -> Int -> Int
-- p n 1 = 1
-- p n 0 = 0

c : Int -> Int -> Int
c n 0 = 1
c n k = 
    if 1 <= k && k <= n-1 then
       fact n `div` ((fact (n - k)) * (fact k))
    else
        -1
    where fact : Int -> Int
          fact 0 = 1
          fact n = n * fact (n - 1)

qaste : Int -> Int -> Int
qaste x 0 = 1
qaste x n =
    if n `mod` 2 == 0 then
        y * y
    else
        x * y * y
    where y : Int
          y = qaste x (n `div` 2)

ndiv : Int -> Int -> Int
ndiv x y = f x 0
    where f : Int -> Int -> Int
          f n z = 
            if n < y then
                z
            else
                f (n - y) (z + 1)

