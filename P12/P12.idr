import Data.IOArray
import Data.List
import Control.App
import Control.App.Console
import Random
 
 
%hide Builtin.(#)
%hide Builtin.DPair.(#)
 
infix 5 #
infix 6 ##
 
data L : Type -> Type -> Type where
    (#) : (1 _ : a) -> b -> L a b
 
    -- Konstrueerimine: lineaarset resrurssi saab panna esimeseks
    -- komponendiks, aga siis on terve paar lineaarne.
 
    -- Mustrisobitusel: kui kogu paar peab olema lineaarne, siis
    -- selle esimene komponent on lineaarne.
 
(Show a, Show b) => Show (L a b) where
    show (x # y) = show x ++ " # " ++ show y
 
data LL : Type -> Type -> Type where
    (##) : (1 _ : a) -> (1 _ : b) -> LL a b
 
    -- Sama, mis L aga mõlemad komponendid lineaarsed.
 
(Show a, Show b) => Show (LL a b) where
    show (x ## y) = show x ++ " ## " ++ show y
 
 
-- Lihtsustus: Double-te massiivid (indekseeritud Nat-dega)
 
public export
interface Array arr where
  write  : (1 a : arr) -> Nat -> Double -> arr
  read   : (1 a : arr) -> Nat -> L arr Double
  size   : (1 a : arr) -> L arr Nat
  frmLst : List Double -> arr
  toLst  : (1 a : arr) -> L () (List Double)
  delArr : (1 a : arr) -> ()
  -- mälu haldamise jaoks on frmLst ebaturvaline, tuleks teha:
  -- withArray : ((1 a : arr) -> L arr c) -> c  

mapArr : Array arr => (Double -> Double) -> (1 _ : arr) -> arr
mapArr f a =
    let a # sizea = size a in
    h sizea a
where
    h : Nat -> (1 _ :arr) -> arr
    h Z     a = a
    h (S n) a = let a # v = read a n
                    a     = write a n (f v)
                in
                h n a
 
copyAll : Array arr => (1 src : arr) -> (1 dst : arr) -> LL arr arr
copyAll src dst =
    let src # sizes = size src in
    let dst # sized = size dst in
    if sizes == sized then
        h sizes src dst
    else
        src ## dst
  where
    h : Nat -> (1 _ :arr) -> (1 _ :arr) -> LL arr arr
    h Z     a b = a ## b
    h (S n) a b = let a # va = read a n
                      b      = write b n va
                  in
                    h n a b
 
data LinArray = MkLinArray (IOArray Double)
 
newIOArr : List Double -> IO (IOArray Double)
newIOArr xs =
    let l = cast (length xs) in
    do a <- newArray l
       let upd = zip [0..l-1] xs
       traverse_ (uncurry $ writeArray a) upd
       pure a
 
toListIOArr : IOArray Double -> IO (List Double)
toListIOArr a =
    let l = Data.IOArray.max a in
    do xs <- traverse (readArray a) [0..l]
       pure (takeWhileJust xs)
  where
    takeWhileJust : List (Maybe b) -> List b
    takeWhileJust (Just x :: xs) = x :: takeWhileJust xs
    takeWhileJust _ = []
 
Array LinArray where
    frmLst xs = MkLinArray $ unsafePerformIO (newIOArr xs)
 
    toLst (MkLinArray a) = unsafePerformIO (do xs <- toListIOArr a
                                               pure (() # xs))
 
    delArr (MkLinArray a) = ()
 
    size (MkLinArray a) = MkLinArray a # (cast (max a))
 
    write (MkLinArray a) i v =
        unsafePerformIO (do ok <- writeArray a (cast i) v
                            pure (MkLinArray a))
    read (MkLinArray a) i =
        unsafePerformIO (do r <- readArray a (cast i)
                            case r of
                                Just v  => pure (MkLinArray a # v)
                                Nothing => pure (MkLinArray a # 0))
 
newLinArr : List Double -> LinArray
newLinArr = frmLst

swap : Array arr => (1 _ : arr) -> Nat -> Nat -> arr
swap a i j =  
    if i == j then a else
    let a # ai = read a i
        a # aj = read a j
        a      = write a i aj
        a      = write a j ai
    in a

loop: Array arr => (1 a : arr) -> (pivot:Double) -> (j:Nat)
                -> (hi:Nat) -> (i:Nat) -> L arr Nat
loop a pivot j hi i = 
    if j>=hi then a # i else
        let a # aj = read a j in
        if (aj < pivot) then 
            let a = swap a j i in
            loop a pivot (j+1) hi (i+1)
        else
            loop a pivot (j+1) hi i
 
test_loop : (List Double, Nat)
test_loop =
    let 1 a     = newLinArr [1,3,2,7,4]
        a # i   = loop a 3 0 3 0  -- viimane element (pivot): 3
        () # al = toLst a         -- esimene indeks: 0
    in                            -- eelviimane indeks: 3
    (al,i)                        -- esimene indeks: 0

decr : Nat -> Nat
decr 0 = 0
decr (S n) = n
 
partition : Array arr => (1 a : arr) -> (lo: Nat) -> (hi: Nat) -> L arr Nat
partition a lo hi = 
    let a # pivot = read a hi
        a # i     = loop a pivot lo (decr hi) lo
        a         = swap a i hi
    in a # i

test_part : (List Double, Nat)
test_part =
    let 1 a     = newLinArr [0,1,4,5,3]
        a # i   = partition a 0 4   -- esimene indeks: 0
        () # al = toLst a           -- viimane indeks: 4
    in
    (al,i)

qs : Array arr => (1 a : arr) -> (lo: Nat) -> (hi: Nat) -> arr
qs a lo hi = 
    if lo >= hi then 
        a
    else
        ?hole
 
quickSort : Array arr => (1 _ : arr) -> arr
quickSort a =
    let a # len = size a in
    qs a 0 (decr len)
 
test_qs : List Double
test_qs =
    let 1 a = newLinArr [5,4,7,6,1,2]
        a   = quickSort a
        () # al = toLst a
    in
    al

public export
interface MyHasIO world where
  writeConsole  : (1 _ : world) -> String -> world
  readConsole   : (1 _ : world) -> L world String
  rnd           : (1 _ : world) -> Int -> Int -> L world Int
 
private
data World : Type where
    MkWorld : World
 
MyHasIO World where
    writeConsole MkWorld xs = unsafePerformIO (do putStrLn xs
                                                  pure MkWorld)
    readConsole MkWorld = unsafePerformIO (do l <- getLine
                                              pure (MkWorld # l))
    rnd MkWorld l h =  unsafePerformIO (do l <- randomRIO (l,h)
                                           pure (MkWorld # l))
 
runIO : Show b => ((1 _ : World) -> L World b) -> IO b
runIO prg =
    let MkWorld # y = prg MkWorld in
    pure y

prog1 : MyHasIO world => (1 w : world) -> L world ()
prog1 w = 
    let w # s = readConsole w
        w # i = rnd w 2 6
        txt   = concat (replicate (cast i) s)
        w     = writeConsole w txt
    in 
        w # ()

-- no good
printArray: (MyHasIO wrld, Array arr)=> (1 a: arr) -> (1 w: wrld) -> LL wrld arr
printArray a w = 
    let a # l  = size a
        w ## a = printElem w a l
    in w ## a
        where printElem : (1 w: wrld) -> (1 a: arr) -> Nat -> LL wrld arr
              printElem w a n = 
                let
                    w ## a = printArray a w
                    a # ai = read a n
                    w      = writeConsole w (cast ai)
                in w ## a
 
testPrint : MyHasIO wrld =>  (1 w: wrld) -> L wrld ()
testPrint w =
    let 1 a    = newLinArr [1,2,3,4]
        w ## a = printArray a w
        ()     = delArr a
    in w # ()
