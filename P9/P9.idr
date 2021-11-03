import Future
import System.Clock
 
time: Lazy a -> IO a
time v = do
    c1 <- clockTime UTC
    let r = v
    c2 <- clockTime UTC
    let d = timeDifference c2 c1
    printLn d
    pure r
 
longFun' : Int -> Int
longFun' x = if x>171000000 then 1 else longFun' (x+1)
 
one_long: () -> Int
one_long () = longFun' 0
 
two_long: () -> Int
two_long () = longFun' 0 + longFun' 0

paralet: () -> Int
paralet () = let x = fork (longFun' 0) in
             let y = fork (longFun' 0) in
             (await x) + (await y)

-- ei tee kiiremaks, sest
-- ootab awaitiga terve ahela tulemust
-- (ahelat täidetakse siiski ükshaaval)
paramonad : () -> Int
paramonad () = await (do x <- fork (longFun' 0)
                         y <- fork (longFun' 0)
                         pure (x + y))

paraapp : () -> Int
paraapp () = await ((+) <$> fork (longFun' 0) <*> fork (longFun' 0))
