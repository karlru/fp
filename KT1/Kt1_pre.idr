import Random

-- Ãœlesanne 1:
-- Funkstsioon yl1 arvutab True siis ja ainult siis, kui 
-- listis leidub paar (a,b), kus a ja b kaugus pole suurem kui 1.0.

yl1 : List (Double, Int) -> Bool
yl1 [] = False
yl1 x  = foldr f False x
    where f : (Double, Int) -> Bool -> Bool
          f (x, y) z = if abs ((cast y)-x) <= 1 then True else z

-- NÃ¤iteks:
-- yl1 [] == False
-- yl1 [(1.1, 2)] == True               -- 2 - 1.1 == 0.9 <= 1.0
-- yl1 [(1.1, 3),(2.2, 2)] == True      -- 2.2 - 2 == 0.2 <= 1.0
-- yl1 [(1.1, 3),(3.0, 0)] == False


-- Ãœlesanne 2:

data Puu a = Leht0 | Leht1 a | Haru (Puu a) a (Puu a) 

-- Funkstsioon yl2 saab argumendiks puu, mille elemendid
-- on paarid (i,x), ja vÃ¤Ã¤rtuse y. Tagastada tuleb listi kÃµik
-- esimesed komponendid i, mis on paaris vÃ¤Ã¤rtusega y (ehk x==y).

yl2 : Eq a => Puu (Int, a) -> a -> List Int
yl2 p y = lahenda p y []
where
    lahenda : Puu (Int, a) -> a -> List Int -> List Int
    lahenda Leht0 y l             = l
    lahenda (Leht1 (i, x)) y l    = 
        if x == y then 
            i :: l
        else l
    lahenda (Haru n (i, x) m) y l = 
        if x == y then 
            lahenda n y (i::l) ++ lahenda m y l
        else lahenda n y l ++ lahenda m y l

-- NÃ¤iteks:
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (2, 'b'))) 'a' == [1]
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (2, 'b'))) 'b' == [2]
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (2, 'b'))) 'c' == []
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (1, 'a'))) 'a' == [1,1]


-- Ãœlesanne 3:

interface Veider a where
    f : a -> Integer
    g : Char -> a

-- Implementeeri liidese Veider instantsid nii, et 
-- kÃµik jÃ¤rgnevad avaldised oleks tÃµesed:

-- a) g 'x' == 10
-- b) f True + f 1 == g 'x'

Veider Char where
    f x = 10
    g x = ?rhs

Veider Bool where
    f x = 5
    g x = False

Veider Integer where
    f x = 5
    g x = 10

-- Ãœlesanne 4:

t2ring : IO Int
t2ring = randomRIO (1,6)

-- Protseduur yl4 kÃ¼sib kasutajalt Ã¼he rea teksti ja trÃ¼kib 
-- selle vÃ¤lja selliselt, et iga sisestatud tÃ¤he kohta trÃ¼kitakse
-- juhuslikult see tÃ¤ht kas Ã¼hekordselt vÃµi kahekordselt.

-- Vihje: kas teate, et on olemas unpack ja putChar
yl4 : IO () 
yl4 = do 
    putStrLn "Sisesta tekst"
    x <- getLine
    prindi (unpack x)
    where
        prindi : List Char -> IO ()
        prindi []        = pure ()
        prindi (x :: xs) = do
            y <- t2ring
            if y > 3 then do
                putChar x
                putChar x
                     else putChar x
            prindi xs

-- Main> :exec yl4
-- Tere hommikust!
-- TTere  hommikuust!
