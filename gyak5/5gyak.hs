import Data.Char

sumEvensSquared :: [Int] -> Int
sumEvensSquared xs = sum [x^2 | x <- xs, even x]

-- append vs (:) -- futasido + memoriaigeny
numericFromTo n m
  | n > m     = []
  | otherwise = n : numericFromTo (n + 1) m

numericFromToAppend n m
  | n > m     = []
  | otherwise = numericFromToAppend n (m - 1) ++ [m]


-- Sorold fel az összes prímszámot!
isPrime :: Integer -> Bool
isPrime x = x > 1 && null [1 | n <- [2..x `div` 2], x `mod` n == 0]

primes :: [Integer]
primes = [x | x <- [2..], isPrime x]

-- Add meg egy parokat (kulcs-ertek) tartalmazo lista elemei kozul az 1-eshez tartozo ertekeket!
ones :: [(Int, a)] -> [a]
ones xs = [snd x | x <- xs, fst x == 1]

ones' :: [(Int, a)] -> [a]
ones' xs = [ y | (x, y) <- xs, x == 1]

ones'' :: [(Int, a)] -> [a]
ones'' xs = [ y | (1, y) <- xs]

-- Csereld fel egy szovegben az angol abecebeli kis es nagybetuket! isLetter - tetszoleges betu
swapCaps :: String -> String
swapCaps xs = [ if isLower x then toUpper x else toLower x | x <- xs, elem x (['a'..'z'] ++ ['A'..'Z'])]

-- Szamjegy-e egy karakter?
isDigit' :: Char -> Bool
isDigit' = undefined

-- Csak pozitiv szamokat tartalmaz-e egy lista?
allPositive :: [Int] -> Bool
allPositive = undefined

-- adatbazisok: listak + n-esek kombinacioja
-- Tancosok adatbazisa: stilusonkent tartalmazza az adott stilusban tancolok nevet, nemet, es hogy
-- epp keres-e tancpartnert az adott stilusban
dancers = [("salsa", [("Kiss", "Samuel", True, "ferfi"),
                      ("Kiralyi", "Evelin", True, "no"),
                      ("Kovacs", "Bela", False, "ferfi"),
                      ("Nemeth", "Csilla", True, "no"),
                      ("Solymos", "Klara", True, "no"),
                      ("Mars", "Bruno", False, "ferfi"),
                      ("Kovacs", "Agoston", False, "ferfi"),
                      ("Nemeth", "Balazs", True, "ferfi")]),
           ("tarsastanc",
                      [("Nagy", "Salamon", False, "ferfi"),
                       ("Janosi", "Janos", False, "ferfi"),
                       ("Elek", "Tibor", False, "ferfi"),
                       ("Nemeth", "Csilla", True, "no"),
                       ("Nagy", "Xenia", True, "no"),
                       ("Musk", "Elon", True, "01100110 01100101 01110010 01100110 01101001"),
                       ("Solymos", "Klara", True, "no"),
                       ("Nemeth", "Balazs", False, "ferfi")]),
           ("west-coast swing",
                      [("Kis", "Sandor", True, "ferfi"),
                       ("Moricz", "Zsigmond", True, "ferfi"),
                       ("Elek", "Tibor", True, "ferfi"),
                       ("Nemeth", "Csilla", True, "no"),
                       ("Nagy", "Sandor", True, "ferfi"),
                       ("Musk", "Elon", True, "01100110 01100101 01110010 01100110 01101001"),
                       ("Solymos", "Klara", True, "no"),
                       ("Nemeth", "Balazs", True, "ferfi")])
          ]

-- Add meg azokat a noket, akik tarsastancban keresnek partnert!

-- Kik azok az emberek, akik legalabb 2 stilust tancolnak?

-- Add meg azokat a tancosokat, akiknek ugyanaz a vezetekneve!

-- Van-e olyan tancstilus, ahol mindenki part keres?



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
electrics xs = sum [count | (brand, dat) <- xs,  (typ, fuel, count) <- dat, fuel == "electric"]




















