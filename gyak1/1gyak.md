- TMS: tms.inf.elte.hu
- Haskell: https://www.haskell.org/ghcup/ (legfrissebb verzió megfelelő)
- Pandora-n is van ghci (v8.0.1)

- Elsősorban az interpretert használjuk: ghci
  - Értelmezi, majd betölti az fájlt/modult
  - Kifejezéseket értékel ki
  - Fontosabb parancsok:
    - ghci           <- elindítja az interpretert
    - :l <fájlnév>   <- modulbetöltés
    - :r             <- betöltött modulok újratöltése
    - :q             <- kilépés
    - :t <kifejezés> <- típuslekérdezés (adatokra)
    - :i <típus>     <- típusinformáció (típusokra)

- Bevezetés:
  - Kísérletezés számokkal
    - alaptípusok (Int, Integer, Float, Double, Char)
    - Int (32/64 bites) vs Integer (bármekkora lehet)
    - műveletek és függvényhívások (+, -, *, /, ^, ^^, **, mod, div)
    - erős típusosság
    - nincsenek változók, csak nevek! Minden név csak egyszer definiálható!
  - konstansok (0 paraméteres függvény), pl. i = 0
  - konstans függvény, pl. f x = 0
  - HF: Kísérletezés alakzatokkal (http://lambda.inf.elte.hu/VISZ2017.xml) <- egyelőre bővebb részletek nélkül
    - <|> olyan "mágikus" operátor, mellyel alakzatokat tudunk kombinálni
    - függvényhívás újra