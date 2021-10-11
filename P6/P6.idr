import Set
import StateT
 
data Term = Var String          -- muutuja
          | App Term   Term     -- rakendus
          | Lam String Term     -- abstraktsioon
          | Con Int             -- täisarvuline väärtus
 
showTerm : Nat -> Term -> String
showTerm _ (Con n)   = show n
showTerm _ (Var x)   = x
showTerm d (App f e) =
    showParens (d>1) (showTerm 1 f++" "++showTerm 2 e)
showTerm d (Lam x e) =
    showParens (d>0) ("\\ "++x++". "++showTerm 0 e)
 
Show Term where
    show = showTerm 0
 
Eq Term where
    (Var x)   == (Var y)   = x==y
    (App x y) == (App z w) = x==z&&y==w
    (Lam x y) == (Lam z w) = x==z&&y==w
    (Con x)   == (Con y)   = x==y
    _         == _         = False

tI : Term
tI = Lam "a" (Var "a")
 
tK : Term
tK = Lam "a" (Lam "b" (Var "a"))
 
tS : Term
tS = Lam "a" (Lam "b" (Lam "c"
        (App (App (Var "a") (Var "c"))
            (App (Var "b") (Var "c")))))
 
test : Term
test = App (App (App tS tI) tI) tI
 
test1 : Term
test1 = (Lam "a" (Var "a")) `App`
    ((Lam "b" (Var "b")) `App` (Lam "c" (Var "c")))
 
add : Term -> Term -> Term
add x y = App (App (Var "add") x) y
 
test2 : Term
test2 = add (Con 1) (Con 2)
 
test3 : Term
test3 = add (add (Con 1) (Con 2)) (add (add (Con 1) (Con 1)) (Con 3))
 
test4 : Term
test4 = App (Lam "x" (add (Var "x") (Var "x"))) (add (Con 1) (Con 2))
 
selfapp : Term
selfapp = Lam "y" (App (Var "y") (Var "y"))
 
test5 : Term
test5 = App (Lam "x" (Con 5)) (App selfapp selfapp)
 
test6 : Term
test6 = Lam "x" (Var "y")
 
test7 : Term
test7 = App tI (Con 5)

test8 : Term
test8 = Var "y"

test9 : Term
test9 = Con 4

-- vaadata loeng3.pdf slaid 35 (vabade muutujate induktiivne definitsioon)
freeVars : Term -> Set String
freeVars (Var x)   = single x
freeVars (App x y) = freeVars x `union` freeVars y
freeVars (Lam x y) = delete x (freeVars y) 
freeVars (Con x)   = empty

SMap : Type -> Type
SMap a = String -> Maybe a
 
emptyMap : SMap a
emptyMap _ = Nothing
 
addFunMap: Eq a => String -> a -> SMap a -> SMap a
addFunMap x v m y = if x==y then Just v else m y

evalSimple1 : SMap Int -> Term -> Maybe Int
evalSimple1 r (Var x) = r x
evalSimple1 r (App (App (Var "add") x) y) =
    do xv <- evalSimple1 r x
       yv <- evalSimple1 r y
       pure (xv+yv)
evalSimple1 r (App (Lam x z) y) =
    do v <- evalSimple1 r y
       evalSimple1 (addFunMap x v r) z
evalSimple1 r (Con x) = pure x
evalSimple1 r _  = Nothing

fail : St Maybe a b
fail = MkSt (\z => Nothing)
 
getVar : String -> St Maybe (SMap Int) Int
getVar x = do r <- get
              case r x of
                Nothing  => fail
                (Just x) => pure x
 
evalSimple2 : Term -> St Maybe (SMap Int) Int
evalSimple2 (Var x) = getVar x
evalSimple2 (App (App (Var "add") x) y) = 
    do xv <- evalSimple2 x
       yv <- evalSimple2 y
       pure (xv + yv)
evalSimple2 (App (Lam x z) y) = 
    do v <- evalSimple2 y
       update (addFunMap x v)
       evalSimple2 z
evalSimple2 (Con x) = pure x
evalSimple2 _ = fail

newVar : St Maybe Int String
newVar = do
    s <- get
    set (s+1)
    pure ("x" ++ show s)

-- vaadata leong4.pdf slaid 45 (substitutsioon)
substSt : Term -> (String, Term) -> St Maybe Int Term
substSt t (x,e) = subst' t
    where fvs : Set String
          fvs = freeVars e
          subst' : Term -> St Maybe Int Term
          subst' (Con y) = pure y
          subst' (Var y) = pure (if x == y then e else y)
          subst' (App y z) = ?subst3
          -- z? newvar!
          subst' (Lam y e1) = ?subst4