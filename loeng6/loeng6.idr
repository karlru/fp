{-
baaskombinaatorid
    I = l x.x
    K = l x y. x
    S = l f g x. f x (g x)
astendamine
    E^0E' = E'
tõeväärtused
    true  = l x y. x
    false = l x y. y
    not   = l t. t false true

-}

{-
seisundi monaad
    State s a -- s on seisundi tüüp; a väärtuse tüüp
-}