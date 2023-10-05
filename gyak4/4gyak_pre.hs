-- FP meme: https://9gag.com/gag/az2vv1N

-- Hoogle tipusokkal

-- lista kifejezesek, literalok
-- listak tipusa, elemek tipusa, stringek
-- [[]]
-- [True, [False]]
-- [[], [[]]]
-- [[(True, 'a')], [(True, 'a'), (False, 'b')]]
-- ([], [])

-- beepitett fuggvenyek hasznalata listakra
-- ++, reverse, nub, sort, take, drop, length, null


-----------------------------
-- intervallum kifejezesek --
-----------------------------

-- [i, j .. k]
  --  i = elso elem
  --  j = potencialis masodik elem (d-tol fugg)
  --  k = potencialis utolso elem (d-tol fugg)
  --  differencia (d) = j - i
-- sorold fel az szamokat m-tol n-ig!
nums1 m n = [m..n]

-- sorold fel a paros szamokat m-tol n-ig!
nums2 m n = if even m
            then [m, m+2..n]
            else [m+1, m+3..n]

-- intervallum kifejezesek tortekre: felso hatar differencia/2 "sugaru kornyezeteben" van az utolso ertek 
floats1 = [1.1, 1.3 .. 2.2]
floats2 = [1.1, 1.3 .. 2.19]

-----------------------
-- halmazkifejezesek --
-----------------------

-- Add meg egy lista paros elemeinek duplajat!
doubleEvens :: [Int] -> [Int]
doubleEvens xs = undefined

-- Mit kene atirni, hogyha nem szeretnenk a paratlanokat kiszurni?

-- Hany darab paratlan eleme van egy listanak?
odds :: [Int] -> Int
odds = undefined

-- add meg az osszes ora-perc part!
clock :: [(Int, Int)]
clock = undefined

-- Sorold fel az összes prímszámot!
primes :: [Integer]
primes = undefined

-- Add meg egy parokat (kulcs-ertek) tartalmazo lista elemei kozul az 1-eshez tartozo ertekeket!
ones :: [(Int, a)] -> [a]
ones xs = undefined

-- Csereld fel egy szovegben a kis es nagybetuket!
swapCaps :: String -> String
swapCaps = undefined

-- Szamjegy-e egy karakter?
isDigit' :: Char -> Bool
isDigit' = undefined

-- Csak pozitiv szamokat tartalmaz-e egy lista?
allPositive :: [Int] -> Bool
allPositive = undefined

-- adatbazisok: listak + n-esek kombinacioja
-- Az alabbi adatbazis egy hasznaltauto-kereskedes adatbazisa.
-- Minden markahoz fel vannak sorolva az elerheto tipusok, uzemanyag tipussal, es egy szammal
-- amely azt mondja meg, hogy hany darab van keszleten
database =
  [ ("KIA", [("Optima", "plug-in", 5), ("Soul", "electric", 2), ("CEE'D", "plug-in", 2), ("CEE'D", "gasoline", 12), ("XCEE'D", "plug-in", 3)])
  , ("Renault", [("Zoe", "electric", 4), ("Megane", "plug-in", 2), ("Megane", "gasoline", 5), ("Captur", "plug-in", 1)])
  , ("Nissan", [("Leaf", "electric", 6), ("Qashqai", "mild-hybrid", 1), ("X-Trail", "gasoline", 2)])
  , ("Opel", [("Corsa", "gasoline", 2), ("Corsa", "electric", 3)])
  , ("Toyota", [("RAV4", "hybrid", 4), ("Camry", "hybrid", 2), ("Corolla", "hybrid", 9), ("Prius", "plug-in", 3)])
  , ("Ssangyong", [("Korando", "diesel", 4), ("Rexton", "diesel", 2)])
  , ("Mazda", [("MX-30", "electric", 1), ("3", "mild-hybrid", 2), ("6", "gasoline", 1)])
  ]
-- mi a fenti adatbazis tipusa?

-- Komplex feladat: Add meg, hogy hany darab elektromos autot arulnak a kereskedesben!
electrics xs = undefined

