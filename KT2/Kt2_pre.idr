%default total 

-- esimese kolme ülesande lahendused eraldi PDF-is

-- 1) leia kõik reedeksid
-- ...
-- 2) beeta-redutseeri normaalkujule normaal- ja aplikatiivjärjekorras
-- ...
-- 3) tüübi tuletamine
-- ...

-- 4) sõltuvate tüüpidega programmeerimine

data Vect : Nat -> Type -> Type where
    Nil  : Vect 0 a
    (::) : a -> Vect n a -> Vect (1+n) a 

topelt : Nat -> Nat 
topelt 0 = 0
topelt (S k) = S (S (topelt k))

-- ex1 -- pane elemendid vaheldumisi  (NB! Kalmeril siin plugin juksib.)
sega : Vect n a -> Vect n a -> Vect (topelt n) a
sega [] [] = []
sega (x :: xs) (y::ys) = x :: y :: sega xs ys

-- Main> sega [1,2,3] [30,20,10]
-- [1, 30, 2, 20, 3, 10]

concat : Vect n a -> Vect m a -> Vect (n+m) a
concat [] ys = ys
concat (x :: y) ys = x :: concat y ys

-- ex2 -- lamenda tabel
flatten : Vect n (Vect m a) -> Vect (n*m) a
flatten []       = []
flatten (x :: y) = 
    let z = flatten y in
    concat x z

-- Main> flatten [[1,2,3],[30,20,10]]
-- [1, 2, 3, 30, 20, 10]

-- ex3 -- viimane element
last : Vect (S n) a -> a
last (x :: [])       = x
last (x :: (y :: z)) = last (y::z)

-- Main> last [1,2,3]
-- 3

-- ex4 -- korda n korda
repeat : (n:Nat) -> a -> Vect n a 
repeat 0 x = []
repeat (S k) x = x :: repeat k x

-- Main> repeat 3 'a'
-- ['a', 'a', 'a']

-- ex5 -- lisa kõigile üks ette
appendAll : (n:Nat) -> Vect n a -> Vect n (Vect m a) -> Vect n (Vect (S m) a)
appendAll 0 [] [] = []
appendAll (S k) (x :: y) (z :: w) = (x :: z) :: appendAll k y w

-- Main> appendAll 3 [1,2,3] [[30],[20],[10]]
-- [[1, 30], [2, 20], [3, 10]]

-- ex6 -- transponeeri
transpose : (n:Nat) -> (m:Nat) -> Vect n (Vect m a) 
            -> Vect m (Vect n a)
transpose 0 m [] = repeat m []
transpose (S k) m (x :: y) = 
    appendAll m x (transpose k m y)

-- Main> transpose 3 2 [[1,2],[3,4],[5,6]]
-- [[1, 3, 5], [2, 4, 6]]



-- 5) tõestamine Idris2-s

infixl 10 /\
data (/\) : Type -> Type -> Type where
    ConI : a -> b
        ------
        -> a /\ b

infixl 11 \/
data (\/) : Type -> Type -> Type where
    DisjIl :   a
            ------
        -> a \/ b

    DisjIr :   b
            ------
        -> a \/ b

VoidE : Void
        ----
    ->    b
VoidE q impossible

--ex7 -- lausearvutus
test1 : (a \/ b) /\ (a -> b) 
        --------------------
    ->          b
test1 (ConI (DisjIl x) y) = y x
test1 (ConI (DisjIr x) y) = x

--ex8 -- lausearvutus
test2 : (a \/ b \/ c) /\ (b -> c) 
        -------------------------
    ->         (a \/ c)
test2 (ConI (DisjIl (DisjIl x)) y) = DisjIl x
test2 (ConI (DisjIl (DisjIr x)) y) = DisjIr (y x)
test2 (ConI (DisjIr x) y) = DisjIr x


-- tuleb kasuks test3-s
mult0 : (k:Nat) -> 0 = k*0
mult0 0 = Refl
mult0 (S k) = mult0 k

-- NB! Nat ei ole tegelikult piiratur ressurss.
add : (1 _ : Nat) -> (1 _: Nat) -> Nat
add x 0 = x 
add 0 (S k) = S k
add (S j) (S k) = 2+j+k

mul : (1 _ : Nat) -> (1 _: Nat) -> Nat
mul 0 0 = 0
mul (S k) 0 = 0
mul 0 (S k) = 0
mul (S j) (S k) = (S j)*(S k)

--ex9 -- mul on korrutamine
test3 : (a:Nat) -> (b:Nat)
        ------------------
    ->     mul a b = a*b
test3 0 0 = Refl
test3 0 (S k) = Refl
test3 (S k) 0 = mult0 k
test3 (S k) (S j) = Refl

--ex9 -- liitmisel S paremal
plus_n_Sm : (n:Nat)  ->  (m:Nat)
            --------------------
         -> n + (S m) = S (n+m)
plus_n_Sm 0 m = Refl
plus_n_Sm (S k) m = 
    -- S (plus k (S m)) = S (S (plus k m))
    rewrite plus_n_Sm k m in                -- plus k (S m) = S (plus k m)
    -- S (S (plus k m)) = S (S (plus k m))
    Refl


--ex10 -- topelt n arvutab n+n
topeltOk :    (n:Nat) 
            --------------
        -> topelt n = n+n
topeltOk 0 = Refl
topeltOk (S k) = 
    -- S (S (topelt k)) = S (plus k (S k))
    rewrite topeltOk k in                  -- topelt k = plus k k 
    -- S (S (plus k k)) = S (plus k (S k))
    rewrite plus_n_Sm k k in               -- plus k (S k) = S (plus k k)
    -- S (S (plus k k)) = S (S (plus k k))
    Refl

--ex11 -- foldr (::) [] on identsusfunktsioon
foldr_id : (xs:List a) -> foldr (::) [] xs = xs
foldr_id [] = Refl
foldr_id (x :: xs) = 
    -- x :: foldr (::) [] xs = x :: xs
    rewrite foldr_id xs in              -- foldr (::) [] xs = xs
    -- x :: xs = x :: xs
    Refl

foldl_rev : (xs:List Nat) -> map (plus 0) xs = xs
foldl_rev [] = Refl
foldl_rev (x :: xs) = 
    -- x :: map (plus 0) xs = x :: xs
    rewrite foldl_rev xs in  
    -- x :: xs = x :: xs
    Refl



-- 6) lineaarsed tüübid

namespace Lin
    public export
    data List : Type -> Type where
        Nil  : Lin.List a
        (::) : (1 _ : a) -> (1 _ : Lin.List a) -> Lin.List a

--ex12 -- rakendab funktsioonid järjest
seqL : (1 _: Lin.List ((1 _ : a) -> a)) -> (1 _: a) -> a
seqL [] x       = x
seqL (y :: z) x = seqL z (y x)

-- Main> seqL [add 1, add 2] 0
-- 3
-- Main> seqL [add 3, mul 0, add 4] 0
-- 4

--ex13 -- foldr, kus akumulaator on lineaarne
foldLf : ((1 _: a) -> b -> a) -> Prelude.List b -> (1 _: a) -> a
foldLf f [] y        = y
foldLf f (x :: xs) y = f (foldLf f xs y) x

-- Main> foldLf (\ x, y => add x y) [1,3] 0
-- 4
-- Main> foldLf (\ x, y => add x y) [2,3] 1
-- 6

--ex14 -- foldr, kus akumulaator ja list on lineaarsed
foldLl : ((1 _ : a) -> (1 _ : b) -> a) -> (1 _ : Lin.List b) -> a -> a
foldLl f [] y       = y
foldLl f (x :: z) y = f (foldLl f z y) x

-- Main> foldLf (\ x, y => mul x y) [2,0] 1
-- 0
-- Main> foldLf (\ x, y => mul x y) [2,5] 1
-- 10

append : (1 _ : Lin.List a) -> (1 _ : Lin.List a) -> Lin.List a
append [] ys       = ys
append (x :: y) ys = x :: append y ys

--ex15 -- lineaarse listi lamendamine
concatL : (1 _ : Lin.List (Lin.List a)) -> Lin.List a
concatL []       = []
concatL (x :: y) = append x (concatL y)

-- Main> concatL [[],[],[]]
-- []
-- Main> concatL [[1,2,3],[],[4,5]]
-- [1, 2, 3, 4, 5]
