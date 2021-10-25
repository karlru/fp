import Random

-- Ülesanne 1:

-- Funkstsioon yl1 arvutab True siis ja ainult siis, kui 
-- listis leidub paar (a,b), kus a ja b on võrdsed.

yl1 : List (Int, Int) -> Bool
yl1 x = foldr f False x
    where f : (Int, Int) -> Bool -> Bool
          f (x, y) z = if x == y then True else z

-- Näiteks:
-- yl1 [] == False
-- yl1 [(1, 1)] == True
-- yl1 [(2, 3),(2, 2)] == True
-- yl1 [(1, 3),(3, 0)] == False


-- Ülesanne 2:

data Puu a = Leht0 | Leht1 a | Haru (Puu a) a (Puu a) 

-- Funkstsioon yl2 saab argumendiks sorteeritud puu ja arvu n. Puust
-- otsitakse kesk-järjekorras esimene paar (i,x), kus i on võrdne 
-- n-ga, ja tagastatakse Just x. Kui sellist paari ei leidu,
-- tagastatakse Nothing.

yl2 : Puu (Int, a) -> Int -> Maybe a
yl2 Leht0 v             = Nothing
yl2 (Leht1 (i, x)) v    = 
    if i==v then 
        Just x 
    else Nothing
yl2 (Haru m (i, x) n) v = 
    if i==v then 
        Just x 
    else 
        if v < i then 
            yl2 m v
        else
            yl2 n v

-- kui puu ei oleks järjestatud asendada real 38 asuva else sisu järgnevaga:
-- case yl2 m v of 
--     Just x => Just x
--     Nothing => yl2 n v

-- Näiteks:
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (2, 'b'))) 1 == Just 'a'
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (2, 'b'))) 2 == Just 'b'
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (2, 'b'))) 3 == Nothing
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (1, 'b'))) 1 == Just 'a'


-- Ülesanne 3:

interface VeiderYks a where
    yks : a 

-- Implementeeri liidese VeiderYks instantsid nii, et 
-- kõik järgnevad avaldised oleks tõesed:

-- a) yks == 1
-- b) yks == True
-- c) yks == [1]

VeiderYks Integer where
    yks = 1

VeiderYks Bool where
    yks = True

VeiderYks (List Integer) where
    yks = [1]

-- Ülesanne 4:

t2ring : IO Int
t2ring = randomRIO (1,6)

-- Protseduur yl4 küsib kasutajalt ühe rea teksti ja hakkab tähti 
-- välja trükkima. Iga tähe juures visatakse täringut. Tähti 
-- trükitakse vaid senikaua, kuni visatakse 1.

-- Vihje: kas teate, et on olemas unpack ja putChar
yl4 : IO () 
yl4 = do
    putStrLn "Sisestage midagi huvitavat:"
    line <- getLine
    tryki (unpack line)
    where
        tryki : List Char -> IO ()
        tryki []        = pure ()
        tryki (x :: xs) = do
            luck <- t2ring
            if luck == 1 then putStrLn ""
                         else do
                            putChar x
                            tryki xs

-- Main> :exec yl4
-- Tere!
-- Te
-- Main> :exec yl4
-- Hello!
-- Hello
-- Main> :exec yl4
-- Hommik!
-- 
