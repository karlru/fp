import System

data Tree a = Leaf | Branch (Tree a) a (Tree a)
 
Eq a => Eq (Tree a) where
    Leaf == Leaf = True
    (Branch x y z) == (Branch w v s) = x==w && y==v && z==s
    _ == _ = False

Functor Tree where
    map f Leaf           = Leaf
    map f (Branch x y z) = Branch (map f x) (f y) (map f z)

Foldable Tree where
  foldr f b Leaf           = b
  foldr f b (Branch x y z) =  foldr f (f y (foldr f b z)) x

len : Foldable t => t a -> Int
len t = ?q

dialoog : IO ()
dialoog = do
    putStr "Sisesta oma nimi: "
    xs <- getLine
    putStrLn ("Tere, "++xs++"!")

prindiArvud : List Int -> IO ()
prindiArvud []      = do
    pure ()
prindiArvud (x::xs) = do
    putStrLn (show x)
    prindiArvud xs

readMaybe : IO (Maybe Int)
readMaybe = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing
 
loeArv : IO Int
loeArv = do 
    putStrLn "Sisesta arv: "
    v <- readMaybe
    case v of
        Nothing => do
            putStrLn "Kahjuks mitte"
            loeArv
        Just n => 
            pure n

summa2 : IO ()
summa2 = do
    arv1 <- loeArv
    arv2 <- loeArv
    putStrLn (show (arv1 + arv2))
    pure ()

summaN1 : IO ()
summaN1 = do
    putStrLn "Liidetavate arv"
    n <- loeArv
    h <- summaRec n
    putStrLn (show(h))
    where
        summaRec : Int -> IO Int
        summaRec 0 = pure 0
        summaRec x = do
            y <- loeArv
            pure (y)
 
summaN2 : IO ()
summaN2 = do
    putStrLn "Liidetavate arv"
    n <- loeArv
    xs <- sequence (map (\a => loeArv) [1..n])
    putStrLn (show (sum xs))

m2ng : IO ()
m2ng = ?rhs_m4ng

data Expr = Const Int | Add Expr Expr | Div Expr Expr
 
expr1 : Expr
expr1 = Div (Add (Const 3) (Const 1)) (Const 2)
expr2 : Expr
expr2 = Add (Const 1) (Div (Const 1) (Add (Const 1) (Const (-1))))
expr3 : Expr
expr3 = Div (Const 5) (Const 1)

evalExpr : Expr -> Maybe Int
evalExpr (Const x) = pure x
evalExpr (Add x y) = do
    x' <- evalExpr x
    y' <- evalExpr y
    pure (x' + y')
evalExpr (Div x y) = do
    x' <- evalExpr x
    y' <- evalExpr y
    if y' == 0 then Nothing else pure (x' `div` y')
