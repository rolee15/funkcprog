import Data.Char
import Data.List

-- @ minta (alias):
f :: Eq a => [a] -> Bool
f l@(x:xs) = l == (x:xs)

-- case kifejezés
-- Valogass szet egy listat az elemek paritasa szerint!
oddEven :: [Int] -> ([Int], [Int])
oddEven [] = ([], [])
oddEven (x:xs) = case oddEven xs of
                    (ys, zs) -> if even x
                                then (x:ys, zs)
                                else (ys, x:zs)

-- Hany darab paratlan eleme van egy listanak?
oddcount :: [Int] -> Int
oddcount xs = length (odds xs)
  where
    odds []     = []
    odds (x:xs) = if odd x
                  then x : odds xs
                  else     odds xs

-- Szurd ki egy lista azon elemeit, amelyeknek a 13-mal vett osztasi maradeka 2!
mod132 :: [Int] -> [Int]
mod132 []     = []
mod132 (x:xs) = if even (x `mod` 13)
                then x : mod132 xs
                else     mod132 xs

-- mi a kozos?
myfilter = undefined

oddcount' = undefined

-- Ehhez szukseg lesz nevtelen fuggvenyekre, vagy fuggvenykompoziciora
{-
  Nevtelen fuggvenyek:
    `f x y = x + y` jelentese igazabol a kovetkezo:
    `f` egy nev, amelyet egy fuggvenyhez kotunk, aminek az eredmenye egy fuggveny:
    `f = \x -> \y -> x + y`, ahol a `\parameter -> torzs` egy nevtelen fuggvenydefinicio.
    Tobb parameter eseten lehet `\x y -> x + y` szintaxist is hasznalni. Felhasznalas, pl.
    `filter (\x -> even x) l` amely egy lista paros elemeit szuri ki, es ez ekvivalens a `filter even l` kifejezessel.

  Eta-ekvivalencia: `\x -> f x` ekvivalens `f`-fel, nem szamit a viselkedes szempontjabol ez a plusz fuggvenyabsztrakcio. Ezt hasznaltuk
  ki a fenti peldaban is `\x -> even x` ekvivalens `even`-nel.
  
  Fuggvenykompozicio: `(.)`
  `(f . g) x = f (g x)`

  Pl. ((+2) . (*3)) 4 = 4 * 3 + 2 = 14
      ((*3) . (+2)) 4 = (4 + 2) * 3 = 18
        ^------ "section"/szelet: https://wiki.haskell.org/Section_of_an_infix_operator


-}

mod123' :: [Int] -> [Int]
mod123' = undefined

-- gyujtsd ki egy lista minden 3. elemet!
everyThird :: [a] -> [a]
everyThird = undefined

-- szorozd meg egy lista paros elemeit kettovel!
double :: [Int] -> [Int]
double [] = []
double (x:xs)
  | even x    = 2 * x : double xs
  | otherwise =     x : double xs

-- Transzormald ASCII kodok listajat szovegge
toString :: [Int] -> String
toString []     = []
toString (x:xs) = chr x : toString xs

ones :: [(Int, a)] -> [a]
ones xs = [b | (a, b) <- xs, a == 1]

onesRec :: [(Int, a)] -> [a]
onesRec []     = []
onesRec ((a, b):xs) = if a == 1
                      then b : onesRec xs
                      else     onesRec xs

mymap = undefined

double' = undefined

toString' = undefined

ones' :: [(Int, a)] -> [a]
ones' xs = undefined


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

-- Komplex feladat: Add meg, hogy hany darab elektromos autot arulnak a kereskedesben! (Mappal es filterrel)
electrics xs = undefined


---------------------------
-- AGGREGACIO/HAJTOGATAS --
---------------------------

-- Add meg egy lista összegét!
summ :: Num a => [a] -> a
summ []     = 0
summ (x:xs) = x + summ xs

-- Add meg egy lista paros elemeinek osszeget!
sumEvens :: [Int] -> Int
sumEvens [] = 0
sumEvens (x:xs) = sumEvens xs + if even x
                                then x
                                else 0

-- Alternativ javaslat filterrel? - fuggvenykompozicio
sumEvens2 = undefined

myfoldr = undefined

summ' = undefined

sumEvens' = undefined

oddEven' = undefined

electrics' xs = undefined



