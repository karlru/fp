import Random

-- Ülesanne 1:
-- Funkstsioon yl1 arvutab True siis ja ainult siis, kui 
-- listis leidub paar (a,b), kus a ja b kaugus pole suurem kui 1.0.

yl1 : List (Double, Int) -> Bool
yl1 = foldr f False
    where f : ?fty
          f = ?fval

-- Näiteks:
-- yl1 [] == False
-- yl1 [(1.1, 2)] == True               -- 2 - 1.1 == 0.9 <= 1.0
-- yl1 [(1.1, 3),(2.2, 2)] == True      -- 2.2 - 2 == 0.2 <= 1.0
-- yl1 [(1.1, 3),(3.0, 0)] == False


-- Ülesanne 2:

data Puu a = Leht0 | Leht1 a | Haru (Puu a) a (Puu a) 

-- Funkstsioon yl2 saab argumendiks puu, mille elemendid
-- on paarid (i,x), ja väärtuse y. Tagastada tuleb listi kõik
-- esimesed komponendid i, mis on paaris väärtusega y (ehk x==y).

yl2 : Eq a => Puu (Int, a) -> a -> List Int
yl2 = ?yl2_rhs

-- Näiteks:
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (2, 'b'))) 'a' == [1]
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (2, 'b'))) 'b' == [2]
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (2, 'b'))) 'c' == []
-- yl2 (Haru Leht0 (1, 'a') (Leht1 (1, 'a'))) 'a' == [1,1]


-- Ülesanne 3:

interface Veider a where
    f : a -> Integer
    g : Char -> a

-- Implementeeri liidese Veider instantsid nii, et 
-- kõik järgnevad avaldised oleks tõesed:

-- a) g 'x' == 10
-- b) f True + f 1 == g 'x'

Veider Integer where
    f = ?q
    g  _ = 10

Veider Bool where
    f = ?e
    g _ = ?r


-- Ülesanne 4:

t2ring : IO Int
t2ring = randomRIO (1,6)

-- Protseduur yl4 küsib kasutajalt ühe rea teksti ja trükib 
-- selle välja selliselt, et iga sisestatud tähe kohta trükitakse
-- juhuslikult see täht kas ühekordselt või kahekordselt.

-- Vihje: kas teate, et on olemas unpack ja putChar
yl4 : IO () 
yl4 = ?yl4_rhs

-- Main> :exec yl4
-- Kalmer
-- KKalmmeerr

-- Main> :exec yl4
-- Tere hommikust!
-- TTere  hommikuust!
