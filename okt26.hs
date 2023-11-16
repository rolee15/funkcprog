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


-- HF: Add meg azokat a tancosokat, akiknek ugyanaz a vezetekneve!
sameName xs = [veznev ++ " " ++ kn | (veznev, kernevek) <- names, length kernevek > 1, kn <- kernevek ] 
  where
    names = [(name1, nub [kernev | (_, dancers') <- xs, (veznev, kernev, _, _) <- dancers', name1 == veznev])
                   | (_, dancers) <- xs, (name1, _, _, _) <- dancers]


-- Add meg az n-edik fibonacci szamot!
fib :: Integer -> Integer
fib 0 = 0 
fib 1 = 1
fib n
  | n > 1     = fib (n - 1) + fib (n - 2)
  | otherwise = error "Negative parameter"


-- Add meg a fibonacci szamok n-edikig tarto listajat! -- !! <- indexeles
fibs :: Integer -> [Integer]
fibs 0 = [0]
fibs 1 = [1, 0]
fibs n
  | n > 1     = head list + head (tail list) : list
  | otherwise = error "Negative parameter"
  where
    list = fibs (n - 1)

-- Végrekurzív fibonacci
tailRecursiveFib :: Integer -> Integer -> Integer -> Integer
tailRecursiveFib 0 n1 n2 = n2
tailRecursiveFib 1 n1 n2 = n1
tailRecursiveFib n n1 n2 = tailRecursiveFib (n - 1) (n1 + n2) n1


database =
  [ ("KIA", [("Optima", "plug-in", 5), ("Soul", "electric", 2), ("CEE'D", "plug-in", 2), ("CEE'D", "gasoline", 12), ("XCEE'D", "plug-in", 3)])
  , ("Renault", [("Zoe", "electric", 4), ("Megane", "plug-in", 2), ("Megane", "gasoline", 5), ("Captur", "plug-in", 1)])
  , ("Nissan", [("Leaf", "electric", 6), ("Qashqai", "mild-hybrid", 1), ("X-Trail", "gasoline", 2)])
  , ("Opel", [("Corsa", "gasoline", 2), ("Corsa", "electric", 3)])
  , ("Toyota", [("RAV4", "hybrid", 4), ("Camry", "hybrid", 2), ("Corolla", "hybrid", 9), ("Prius", "plug-in", 3)])
  , ("Ssangyong", [("Korando", "diesel", 4), ("Rexton", "diesel", 2)])
  , ("Mazda", [("MX-30", "electric", 1), ("3", "mild-hybrid", 2), ("6", "gasoline", 1)])
  ]

carsPerBrand xs = [(brand, sum [count | (_, _, count) <- cars]) | (brand, cars) <- xs]





