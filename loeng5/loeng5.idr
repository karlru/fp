{-
reduktsioon
    järjekord ei oma tähtsust
        church-rosseri teoreem:
            (confluence)
            iga lambda termi e0, e1, e2 korral kui e0 ->>b e1 ja e0 ->>b e2, siis leidub selline e3, et e1 ->>b e3 ja e2 ->>b e3
            järeldus:
                lambda termide normaalkujud on unikaalsed
            normaalkuju leidumine ei ole garanteeritud!
    järjekord on oluline
        normaaljärjekord
            alati redutseerida välimine vasakpoolne reedeks
        aplikatiivne järjekord
            alati redutseerida sisemine vasakpoolne reedeks
-}

{-
tüübiklassid
    tihti tuleb kirjutada funktsioone, mis erinevad ainult tüübi poolest:
        equalChar   : Char -> Char -> Bool
        equalInt    : Int -> Int -> Bool
        equalString : String -> String -> Bool
    seda ei saa kirjutada parameetrilise polümorfse funktsiooniga, kuna funktsioonide implementatsioon on erinev
    selleks saab teha aga liidese (Haskellis tüübiklass)
        interface Equal a where
            (==) : a -> a -> Bool
        Equal Int where
            a == b = ... #intide võrdsus
        Equal Bool where
            a == b = ... #boolide võrdsus
-}

{-
sisend-väljund idrises

idrise puhtad funktsioonid ei võimalda teha mittepuhtaid arvutusi
    puhas - funktsiooni tulemus sõltub ainult argumentide väärtusest
    ei saa teha näiteks juhuarvude funktsiooni

lahendus: IO monaad
    võtab ühe parameetri
    'IO a' tüübi väärtus - "masin, mis arvutab a tüüpi väärtuse"

    pure : a -> IO a                      - masin tagastab esimese argumendi väärtuse
    (>>=) : IO a -> (a -> IO b) -> IO b   - masin käivitab esimese argumendi ja rakendab tulemuse teisele

    lisaks baasfunktsioonid nagu putStrLn : String -> IO () ja getLine : IO String
-}
