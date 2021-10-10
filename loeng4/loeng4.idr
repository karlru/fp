{-
midagi ei saa aru

uute tüüpide loomine idrises
kaks viisi
    type (tüübisünonüüm)
        Pikkus : Type
        Pikkus = Int
    data
        data Bool = True | False
    
-}

Radius : Type
Radius = Double

Height : Type
Height = Double

Width : Type
Width = Double

data Shape = Circle Radius | Rect Width Height

area : Shape -> Double
area (Circle r) = pi * (r*r)
area (Rect w h) = w * h

s1 : Shape
s1 = Circle 1.5

test : Double
test = area s1 + area (Rect 0.73 3.00)

record Point where
    constructor MkPoint
    x, y, z : Double

