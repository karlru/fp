-- if/else
fact1 : Int -> Int
fact1 n = if n==0 then 1 else n * fact1 (n-1)

-- n2idiste sobitamine
fact2 : Int -> Int
fact2 0 = 1
fact2 n = n * fact2 (n-1)

-- with konstruktsioon
fact3 : Int -> Int
fact3 n with (n==0)
    fact3 n | True = 1
    fact3 n | False = n * fact3 (n-1)

-- where konstruktsioon
fact4 : Int -> Int
fact4 n = fact4' 1 n
    where fact4 : Int-> Int -> Int
        fact4' a 0 = a
        fact4' a m = fact4' (a*m) (m-1)