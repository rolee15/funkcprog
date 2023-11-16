-- TMS: tms.inf.elte.hu
-- Fajl/modul betoltese -> :l(oad)
-- Modul ujratoltes     -> :r(eload)

{-  
  Tobbsoros komment
-}


-- Binaris (:set -XBinaryLiterals), hexadecimalis, oktalis literalok

{-
  0xAE
  0o17
  0b111
-}

-- Definialj egyszeru fuggvenyeket: six, tobb tipussal

six :: Integer
six = 6

six' :: Float
six' = 6

six'' :: Double
six'' = 6

const' x = "konstans"

-- Definiald a double fuggvenyt!

{-
int doub(int x) {
  return x * 2;
}
-}

double :: Int -> Int
double x = x * 2

-- Definiald az even fuggvenyt! Lehetseges-e? Meg tudod hivni?

even' x = mod x 2 == 0   -- == -> egyenlosegvizsgalat, /= -> nem egyenloseg vizsgalata
--        mod x 2 /= 1

-- logikai fuggvenyek
-- Megszerkesztheto-e egy haromszog a harom oldala alapjan?

isATriangle a b c = a + b > c && b + c > a && a + c > b

-- Pitagoraszi szamharmas-e a 3 megadott parameter?

isPythagoreanTriplet = undefined

-- Szorzat tipus: rendezett n-es (~~~~ rekordok)
-- a / b ~~> (a, b)

-- Adj ossze ket racionalis szamot!
addRational :: (Int, Int) -> (Int, Int) -> (Int, Int)
addRational x y = (fst x * snd y + fst y * snd x, snd x * snd y)

addRational' :: (Int, Int) -> (Int, Int) -> (Int, Int)
addRational' (x1, x2) (y1, y2) = (x1 * y2 + y1 * x2, x2 * y2)

-- Szorozz ossze ket racionalis szamot!

-- Adott egy helyvektor. Nyujtsd meg konstanszorosara!

stretch :: (Int, Int) -> Int -> (Int, Int)
stretch (x, y) d = (x * d, y * d)

-- Szamtipusok kozotti "konverzio"
-- Kerekitesek: round, floor, ceiling, truncate
-- Egeszek tortkent valo hasznalata: fromInteger, fromIntegral

-- adjuk meg egy szam duplajanak gyoket
f x = sqrt (fromIntegral (double x))

-- Szamold ki x y-nal vett maradekanak gyoket!

-- Oldd meg az egyutthatoival megadott masodfoku egyenletet!

