import Data.List
import Data.Nat

data Email = E String String
 
varmo     : Email
kalmer    : Email
karoliine : Email
karl      : Email
varmo     = E "varmo.vene"       "ut.ee"
kalmer    = E "kalmer.apinis"    "ut.ee"
karoliine = E "karoliine.holter" "ut.ee"
karl      = E "karlkevin.ruul"   "gmail.com"

Show Email where
  show (E nimi domeen) = nimi++"@"++domeen

record Kiri where
  constructor K
  saatja   : Email
  saajad   : List Email
  pealkiri : String
  sisu     : String

testkiri : Kiri
testkiri = K { saatja = karl, 
               saajad = [varmo], 
               pealkiri = "Test", 
               sisu = "email"}

pealkiriSisu : Kiri -> String
pealkiriSisu k = k.pealkiri ++ ": " ++ k.sisu
-- võib ka pealkiriSisu K a b c d = ?rhs_pealkiriSisu
-- võib ka pealkiriSisu k = pealkiri k ++ ": " ++ sisu k

data Tree a = Leaf | Branch (Tree a) a (Tree a)

height : Tree a -> Int
height Leaf             = 0
height (Branch t1 x t2) = 1 + max (height t1) (height t2)

Eq a => Eq (Tree a) where
    Leaf == Leaf                         = True
    Leaf == (Branch t1 x t2)             = False
    (Branch t1 x t2) == Leaf             = False
    (Branch t1 x t2) == (Branch t3 y t4) = 
      if x == y then
        t1 == t3 && t2 == t4
      else
        False

fold : (a -> b -> a -> a) -> a -> Tree b -> a
fold br le Leaf = le
fold br le (Branch t1 x t2) =
  br (fold br le t1) x (fold br le t2)

size : Tree a -> Nat
size Leaf                 = 0
size (Branch t1 x t2)     = 1 + size t1 + size t2

heightFold : Tree a -> Int
heightFold = 
  fold (\ a, _, c => 1 + max a c ) 0

memberOf : Eq a => a -> Tree a -> Bool
memberOf a Leaf             = False
memberOf a (Branch t1 x t2) = a == x || memberOf a t1 || memberOf a t2

balanced : Tree a -> Bool
balanced Leaf = True
balanced (Branch t1 x t2) = 
  abs (height t1 - height t2) <= 1 && balanced t1 && balanced t2

gen : Int -> a -> Tree a
gen 0 y = Leaf
gen x y = (Branch (gen (x - 1) y) y (gen (x - 1) y))

tree2list : Tree a -> List a
tree2list Leaf             = []
tree2list (Branch t1 x t2) = tree2list t1 ++ [x] ++ tree2list t2

list2tree : List a -> Tree a
list2tree [] = Leaf
list2tree x  = treeRec (splitAt ((div) (length x) 2) x)
  where
    treeRec : (List a, List a) -> Tree a
    treeRec ([], [])    = ?rhs2
    treeRec (x::xs, []) = ?rhs
    treeRec (x::[], []) = (Branch Leaf x Leaf)
    treeRec (x, y::ys)  = (Branch (list2tree x) y (list2tree ys))
