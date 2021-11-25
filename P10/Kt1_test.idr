-- uusi tüüpe saab luua kahel viisil
Radius : Type
Radius = Double

Width : Type
Width = Double

Height : Type
Height = Double

data Shape = Rectangle Width Height
		   | Circle Radius

-- sellisel juhul mustrisobitus
area : Shape -> Double
area (Circle r) 	 = 3.14 * r * r
area (Rectangle w h) = w * h

-- liida stringe, liste jnejne
listConcat : List Int
listConcat = [1..5] ++ [6..10]

stringConcat : String
stringConcat = "abc" ++ "def"

-- maybe monaad
-- definitsioon : data Maybe a = Nothing | Just a
inArr : Int -> List Int -> Maybe Int
inArr x [] = Nothing
inArr x (y :: ys) = if x==y then Just x else inArr x ys

-- either
inArr2 : Int -> List (Int, a) -> Either String a
inArr2 x [] 	   = Left "Ei leitud"
inArr2 x ((y,z)::ys) = if x==y then Right z else inArr2 x ys

-- kirjed (erisüntaks algebra jaoks)
record Point where
	constructor MkPoint
	x,y,z : Double

record Sphere where
	constructor MkSphere
	center : Point
	radius : Double

-- saab luua järjestades konstruktori argumendid...
point1 : Point
point1 = MkPoint 5 10 12

-- või anda argumendid nimeliselt
sphere1 : Sphere
sphere1 = MkSphere { radius = 5, center = point1 }
-- sellest saab ka parameetreid kätte, nt sphere1.center.x

-- uut kirjet saab luua vana põhjal
recenter : Sphere -> Point -> Sphere
recenter c p = record { center = p } c

-- tüübiklassid/liidesed - kasutuses nt erinevate tüüpidega sama ideelise funktsiooni elluviimisel
interface Longer a where
	f : a -> a -> Bool

Longer Integer where
	f a b = a > b

Longer String where
	f a b = length a > length b

-- sisseehitatud Eq kasutamine
inArr3 : Eq a => a -> List (a, b) -> Maybe b
inArr3 x [] 		   = Nothing
inArr3 x ((y, z):: ys) = if x == y then Just z else inArr3 x ys 

-- sisend-väljund ning do süntaks (IO monaad)
nimePikkus : IO ()
nimePikkus = do
	nimi <- getLine
	putStrLn (show (length nimi))

-- list comprehension
nullid : List Int -> Int
nullid xs = cast (length [x | x <- xs, x == 0])