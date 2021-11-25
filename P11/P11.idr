%default total 
 
infixl 10 /\
data (/\) : Type -> Type -> Type where
    ConI : a -> b
           ------
        -> a /\ b

conEl : a /\ b
        ------
     ->   a
conEl (ConI x y) = x

conEr : a /\ b
        ------
     ->   b
conEr (ConI x y) = y

infixl 11 \/
data (\/) : Type -> Type -> Type where
    DisjIl :   a
             ------
          -> a \/ b
 
    DisjIr :   b
             ------
          -> a \/ b
 
disjE : (a\/b)  ->  (a -> c)  ->  (b -> c) 
        ----------------------------------
    ->                 c
disjE (DisjIl x) f g = f x
disjE (DisjIr x) f g = g x

%hide Not 
 
data Not : Type -> Type where
    NotI :   (a -> Void)
             -----------
          ->    Not a
 
NotE : Not a -> a
       ----------
    ->    Void
NotE (NotI f) = f
 
--data Void : Type where
---- meelega jäetud tühjaks
 
VoidE : Void
        ----
    ->    b
VoidE q impossible

ex1 :  a /\ (b -> c) /\ (a -> b)
       -------------------------
    ->            c
ex1 (ConI (ConI x z) y) = z (y x)

-- todo
ex2 :      a \/ Not a
       --------------------
    -> (a -> b) \/ (b -> a)
ex2 (DisjIl x) = DisjIr (\y => x)
ex2 (DisjIr (NotI f)) = DisjIr ?h

data Even : Nat -> Type where
    Even_Zero : --------
                 Even 0
 
    Even_Succ :    Even n
                ------------
              -> Even (2+n)
 
even4 : Even 4
even4 = Even_Succ (Even_Succ Even_Zero)
 
even8 : Even 8
even8 = Even_Succ (Even_Succ (even4))
 
plusEvenEven :  Even n -> Even m
               ------------------
             ->    Even (n+m)
plusEvenEven Even_Zero          m_even = m_even
plusEvenEven (Even_Succ n_even) m_even = 
    Even_Succ (plusEvenEven n_even m_even)

multEvenEven :  Even n -> Even m
               ------------------
             ->    Even (n*m)
multEvenEven Even_Zero          m_even = Even_Zero
multEvenEven (Even_Succ n_even) m_even = 
    plusEvenEven m_even $
    plusEvenEven m_even $
    multEvenEven n_even m_even

data Odd : Nat -> Type where
    Odd_one : --------
               Odd 1
 
    Odd_Succ :    Odd n
                -----------
              -> Odd (2+n)
 
odd7 : Odd 7
odd7 = Odd_Succ (Odd_Succ (Odd_Succ Odd_one))

evenOdd :   Even n
          ----------
        -> Odd (1+n)
evenOdd Even_Zero          = Odd_one
evenOdd (Even_Succ n_even) = Odd_Succ (evenOdd n_even)
 
oddEven : Odd n
        ----------
     -> Even (1+n)
oddEven Odd_one          = Even_Succ Even_Zero
oddEven (Odd_Succ n_odd) = Even_Succ (oddEven n_odd)
 
plusOddOdd :  Odd n  ->  Odd m
             -------------------
           ->     Even (n+m)
plusOddOdd Odd_one          m_odd = oddEven m_odd
plusOddOdd (Odd_Succ n_odd) m_odd = Even_Succ (plusOddOdd n_odd m_odd)
 
 
plusEvenOdd :  Even n  ->  Odd m
             -------------------
           ->     Odd (n+m)
plusEvenOdd Even_Zero          m_odd = m_odd
plusEvenOdd (Even_Succ n_even) m_odd = Odd_Succ (plusEvenOdd n_even m_odd)
 
plusNullOdd :    Odd m
              ------------
            -> Odd (m+0)
plusNullOdd Odd_one          = Odd_one
plusNullOdd (Odd_Succ m_odd) = Odd_Succ (plusNullOdd m_odd)

plusNull :   (n:Nat)
            ---------
          -> n+0 = n
plusNull 0     = Refl
plusNull (S k) = rewrite plusNull k in Refl

plusAssoc : (m:Nat) -> (n:Nat) -> (q:Nat)
            ------------------------------
          ->  m + (n + q) = (m + n) + q
plusAssoc 0 n q     = Refl
plusAssoc (S k) n q = rewrite plusAssoc k n q in Refl
