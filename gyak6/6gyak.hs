-----------------
-- ADATBÁZISOK --
-----------------

import Data.List

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
socialDancingWomen xs = [name1 ++ " " ++ name2 | ("tarsastanc", dancers) <- xs, (name1, name2, True, "no") <- dancers]


-- Kik azok az emberek, akik legalabb 2 stilust tancolnak?
twoStyles xs = nub (names \\ nub names)
  where
    names = [(name1, name2) | (_, dancers) <- xs, (name1, name2, _, _) <- dancers]


-- HF: group függvénnyel a Data.List-ből

-- Add meg azokat a tancosokat, akiknek ugyanaz a vezetekneve!



aWords :: String -> [String]
aWords xs = [word | word <- words xs, word !! 0 == 'a' ] -- head

-----------------
--- REKURZIO ----
-----------------

-- Add meg egy szam n-edik faktorialisat!
-- n! = n * (n - 1) * ... * 1
-- n! = n * (n-1)!
fact n
  | n == 0 = 1
  | n > 0  = n * fact (n-1)
  | n < 0  = error "Negative number" -- | otherwise


-- Add meg az n-edik fibonacci szamot!
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Add meg a fibonacci szamok listajat az n-edikig!
fibs 0 = [0]
fibs 1 = [1, 0]
fibs n = head list + head (tail list) : list
  where
    list = fibs (n-1)

-----------------
-- UJRA LISTAK --
-----------------

-- hany fajta listat ismersz?

head' = undefined

tail' = undefined


-- ures-e egy lista?

-- egyelemu-e egy lista?
isSingletonList = undefined

-- ketelemu-e egy lista?


-- stb. Hogyan lehetne megirni azt, hogy 139 elemu-e egy lista?


-- rekurzio: nezzuk meg egyesevel kibontva a beta-redukciokat!
-- szorozd ossze a lista elemeit!


-- Add meg egy lista minden 3. elemet!


-- szorozd meg egy lista paros elemeit kettovel!


-- Hany darab paratlan eleme van egy listanak?


-- Add meg egy lista n-edik elemet! (error fuggveny)


-- Fuzz ossze ket listat! (zip fuggveny)


-- Add meg az osszefesulo rendezes osszefesulo lepeset (merge fuggveny)!
-- Ebben a lepesben ket listat kell rendezetten osszefesulni.

