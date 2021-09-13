{-
literaalid:
    42
    3.1415
    'a' (ainult üks täht)
    "Idris"

operaatorid:
    5+7
    (6 - 3) * 2 + 4

funktsiooni aplikatsioon
    sqrt 2
    abs (-3)
    max (4+3) 6  

tüübikontroll 
    the Int 12 -> kas 12 saab olla tüüp Int

primitiivtüübid
    Int     - fikseeritud täpsusega täisarvud
    Integer - suvalise täpsusega täisarvud
    Double  - topelttäpsusega ujukomaarvud
    Char    - tähemärgid (unicode)
    String  - sõned

lihtsad eeldefineeritud tüübid
    Bool - tõeväärtus
    Nat  - naturaalarvud

ennikud (tuple)
    the (Double, String, Char) (1.1, "string", 'A')
    üheelemendilisi ennikuid pole olemas kuid () on ennik

listid
    the (List Int) [1, 2, 3, 4, 6, 8]
-}

-- näidiste sobitamine:
grade2num : Char -> Int
grade2num 'A' = 5
grade2num 'B' = 4
grade2num 'C' = 3
grade2num 'D' = 2
grade2num 'E' = 1
grade2num 'F' = 0
grade2num  _  = -1
--         ^
--  võib olla ükskõik mis muutuja,
--  kuid _ on tava

-- infix
-- koosnevad sümbolitest :+-*\/=.?|&><!@$%~^#
-- seatakse nn fixity deklaratsioonidega
--  infixl 8 +, -
--  infixl 9 *, /
--  infixl 5 &&
-- fixity deklaratsioone saab vaadata :doc (+)
add3 : Int -> Int
add3 x = (+) x 3
-- sama, mis x + 3

