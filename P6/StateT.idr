module StateT

public export
record St (m: Type -> Type) (s: Type) (a: Type) where
    constructor MkSt
    runSt : s -> m (a,s) 

uc : (a -> b -> c) -> (a,b) -> c
uc = uncurry

export
Monad m => Functor (St m s) where
    map f (MkSt g) = 
        MkSt (\z => do (x,y) <- (g z)
                       pure (f x, y)
                )
-- extra exercise: make it pointless :)

export
Monad m => Applicative (St m s) where
    pure x = MkSt (\z => pure (x,z))
    (MkSt f) <*> (MkSt g) = 
        MkSt (\z => do (h,e) <- f z 
                       (r,t) <- g e 
                       pure (h r,t)
                )
-- extra exercise: make it pointless :)

export
Monad m => Monad (St m s) where
    (MkSt g) >>= f = 
        MkSt (\z => do (x,y) <- g z
                       runSt (f x) y
                )
-- extra exercise: make it pointless :)


export
get : Monad m => St m s s
get = MkSt (\z => pure (z,z))

export
set : Monad m => s -> St m s ()
set w = MkSt (\z => pure ((),w))

export
update : Monad m => (s -> s) -> St m s ()
update f = MkSt (\z => pure ((),f z))