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

tI : Term    -- show tI == "\ a. a"
tI = Lam "a" (Var "a")
 
tK : Term    -- show tK == "\ a. \ b. a"
tK = Lam "a" (Lam "b" (Var "a"))
 
tS : Term    -- show tS == "\ a. \ b. \ c. a c (b c)"
tS = Lam "a" (Lam "b" (Lam "c"
        (App (App (Var "a") (Var "c"))
            (App (Var "b") (Var "c")))))

test0 : Term
test0 = App (Lam "x" (App (Var "x") (Var "x"))) (App (Lam "x" (Var "x")) (Lam "x" (Var "x")))

test : Term   -- "(\ a. \ b. \ c. a c (b c)) (\ a. a) (\ a. a) (\ a. a)"
test = App (App (App tS tI) tI) tI

test1 : Term  -- "(\ a. a) ((\ b. b) (\ c. c))"
test1 = (Lam "a" (Var "a")) `App`
    ((Lam "b" (Var "b")) `App` (Lam "c" (Var "c")))
 
add : Term -> Term -> Term
add x y = App (App (Var "add") x) y
 
test2 : Term  -- show test2 == "add 1 2"
test2 = add (Con 1) (Con 2)
 
test3 : Term  -- "add (add 1 2) (add (add 1 1) 3)"
test3 = add (add (Con 1) (Con 2)) (add (add (Con 1) (Con 1)) (Con 3))
 
test4 : Term  -- show test4 == "(\ x. add x x) (add 1 2)"
test4 = App (Lam "x" (add (Var "x") (Var "x"))) (add (Con 1) (Con 2))
 
selfapp : Term
selfapp = Lam "y" (App (Var "y") (Var "y"))
 
test5 : Term  -- "(\ x. 5) ((\ y. y y) (\ y. y y))"
test5 = App (Lam "x" (Con 5)) (App selfapp selfapp)
 
test6 : Term  -- show test6 == "\ x. y"
test6 = Lam "x" (Var "y")
 
test7 : Term  -- show test7 == "(\ a. a) 5"
test7 = App tI (Con 5)

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
          subst' (Con y) = pure (Con y)
          subst' (Var y) = if x == y then
                            pure e
                           else
                            pure (Var y)
          subst' (App y z) = do y' <- subst' y
                                z' <- subst' z
                                pure (App y' z')
          subst' (Lam y e1) = if x == y then
                                pure (Lam y e1)
                              else if mem y fvs then 
                                do z <- newVar
                                   e' <- substSt e1 (y, Var z)
                                   e'' <- subst' e'
                                   pure (Lam z e'')
                              else
                                do e1' <- subst' e1
                                   pure (Lam y e1')
                
canContract : Term -> Bool
canContract (App (Lam x z) y)                       = True
canContract (App (App (Var "add") (Con z)) (Con y)) = True
canContract _                                       = False

contract : Term -> St Maybe Int Term
contract (App (Lam x z) y)                       = substSt z (x, y)
contract (App (App (Var "add") (Con z)) (Con y)) = pure (Con (z+y))
contract _                                       = fail

focusA : Term -> Maybe (Term, Term -> Term)
focusA (App x y) = 
    case focusA x of
        Just (r,c) => Just (r, \ t => App (c t) y)
        Nothing =>
            case focusA y of
                Just (r,c) => Just (r, \ t => App x (c t))
                Nothing =>
                    if canContract (App x y) then
                        Just (App x y, \ t => t)
                    else
                        Nothing
focusA (Lam x y) = 
    case focusA y of
        Just (r,c) => Just (r, \ t => Lam x (c t))
        Nothing => Nothing
focusA (Var x) = Nothing
focusA (Con x) = Nothing

-- ma kahjuks ei oska...
focusN : Term -> Maybe (Term, Term -> Term)
focusN (App x y) = ?rhs1
focusN (Lam x y) = ?rhs2
focusN (Var x) = Nothing
focusN (Con x) = Nothing

step : (Term -> Maybe (Term, Term -> Term)) -> Term -> St Maybe Int Term
step focus t = 
    case focus t of
        Just (r, c) => do r' <- contract r
                          pure (c r')
        Nothing => fail
