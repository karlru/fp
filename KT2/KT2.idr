-- 1) paberil
-- 2) paberil
-- 3) paberil
-- 4)

data Vect : Nat -> Type -> Type where
    Nil  : Vect 0 a
    (::) : a -> Vect n a -> Vect (1+n) a 

Eq a => Eq (Vect n a) where
    (==) [] [] = True
    (==) (x::xs) (y::ys) = x==y && xs==ys

about_half : Nat -> Nat 
about_half 0         = 0
about_half 1         = 1
about_half (S (S k)) = 1 + about_half k

total
pooled : Vect n a -> Vect (about_half n) a
pooled []             = []
pooled (x :: [])      = [x]
pooled (x :: y :: xs) = x :: pooled xs

-- 5)

infixl 10 /\
data (/\) : Type -> Type -> Type where
    ConI : a -> b
           ------
        -> a /\ b

total
ConEl : a /\ b
        ------
      ->   a
ConEl (ConI x y) = x

total
ConEr : a /\ b
        ------
      ->   b
ConEr (ConI x y) = y

infixl 11 \/
data (\/) : Type -> Type -> Type where
    DisjIl :   a
             ------
          -> a \/ b

    DisjIr :   b
             ------
          -> a \/ b

DisjE : a\/b  ->  (a->c)  ->  (b->c)
        ----------------------------
     ->              c
DisjE (DisjIl x) q w = q x
DisjE (DisjIr x) q w = w x

VoidE : Void
        ----
    ->    b
VoidE q impossible


total
ex5 :  (a -> b /\ c) /\ (b -> a)
       -------------------------
    ->         (b -> c)
ex5 (ConI x y) z = ConEr (x (y z))

-- 6)

liida : Nat -> Nat -> Nat
liida 0 y         = y
liida (S 0) y     = S y
liida (S (S k)) y = S (S (liida k y))

total
mapLiida0 :    (xs:List Nat) 
           ---------------------
        -> map (liida 0) xs = xs
mapLiida0 []        = Refl
mapLiida0 (x :: xs) = rewrite mapLiida0 xs in Refl
