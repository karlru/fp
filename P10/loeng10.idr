-- Paneme tähele, et tüübid võivad sõltuda tüüpidest.

-- Main> :t List
-- Prelude.List : Type -> Type
-- Main> :t IO
-- PrimIO.IO : Type -> Type


-- (Moodle) Kas järgmine funktsioon on korrektne Idris2?
-- Arv : Bool -> Type
-- Arv False = List Int 
-- Arv True  = Int      

-- Tüüp mis sõltub väärtusest:
Arv : Bool -> Type
Arv False = List Int -- False: mittedeterministlik
Arv True  = Int      -- True: deterministlik

test1 : Arv True 
test1 = 5

test2 : Arv False
test2 = [5..10]

plusDet : Arv True -> Arv True -> Arv True
plusDet x y = x+y
-- plusDet test1 test1

plusNonDet : Arv False -> Arv False -> Arv False
plusNonDet xs ys = [x+y|x<-xs, y<-ys]
-- plusNonDet test2 test2

plusSama : (b:Bool) -> Arv b -> Arv b -> Arv b
plusSama False = plusNonDet
plusSama True  = plusDet
-- plusSama True test1 test1 
-- plusSama False test2 test2


-- See on tüütu. Arv b -st võiks b väärtuse tuletada.
-- Tuletamine on Idrises võimalik.

plusSama2 : {b:Bool} -> Arv b -> Arv b -> Arv b
plusSama2 {b = False} = plusNonDet
plusSama2 {b = True}  = plusDet
-- plusSama2 test1 test1  
-- Peaks töötama, aga kuna sisemiselt test1 : Int, ei oska tuletada, et b=True

-- Võrduse ja data erinevus: 
-- + Võrdus lihtustatakse: Arv True --> Int
-- + data-t ei lihtustata, see on normaalkujul

-- Teine katse:
data Arv2 : Bool -> Type where
    Det    :      Int -> Arv2 True   
    NonDet : List Int -> Arv2 False

test3 : Arv2 True 
test3 = Det 5

test4 : Arv2 False
test4 = NonDet [5..10]

-- Nüüd peaks saama tuletada!

plusSama3 : Arv2 b -> Arv2 b -> Arv2 b
plusSama3 (Det x) (Det y) = Det (x+y)
plusSama3 (NonDet xs) (NonDet ys) = 
    NonDet (plusNonDet xs ys)
-- plusSama3 test3 test3  
-- plusSama3 test4 test4  

-- Näide: pikkusega listid

-- Tavalised listid (uue süntaksi abil)
-- %hide List 
-- data List : Type -> Type where
--     Nil  : List a
--     (::) : a -> List a -> List a

-- Paneme Nat-id külge
data Vec : Nat -> Type -> Type where
    Nil  : Vec 0 a
    (::) : a -> Vec n a -> Vec (1+n) a

test5 : Vec 3 Int
test5 = [1,2,3]
-- test5 = 1 :: 2 :: 3 :: Nil

-- Näide1: konkateneerimine:
concatVec : Vec n a -> Vec m a -> Vec (n+m) a
concatVec [] ys = ys
concatVec (x :: y) ys = x :: concatVec y ys

-- Näide2: Vektorite map (teeme loengus)
Functor (Vec n) where
    map f []       = []
    map f (x :: y) = f x :: map f y

-- Näide3: ümberpööramine
-- Ümberpööramine listidel:
revList : List a -> List a
revList [] = [] 
revList (x :: ys) = (revList ys) ++ [x]

-- n + 1  and  1 + n
idVec : {n:Nat} -> Vec (n + 1) a -> Vec (1 + n) a
idVec {n = 0}     xs        = xs
idVec {n = (S k)} (x :: ys) = x :: idVec ys

revVec : {n:Nat} -> Vec n a -> Vec n a
revVec {n=0} [] = []
revVec {n=S k} (x :: ys) = 
    idVec ((revVec ys) `concatVec` [x])

