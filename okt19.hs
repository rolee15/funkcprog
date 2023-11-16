
-- Hogyan kell zárójelezni egy kifejezést?

zarojelezetlen = 10 `rem` 3 == 3^2^1 || 8 > 23 `mod` 9 `div` 2 && False || True
zarojelezett   = (((10 `rem` 3) == (3^(2^1))) || ((8 > ((23 `mod` 9) `div` 2) && False) || True))

-- Melyik az a legkisebb x egész szám, amely esetén az f(x) értéke nagyobb mint 1000? Adjuk meg azt a kifejezést, amely kiszámolja ezt!
-- f(x) = 3x^5 + 4x^3 + 7x^2 + 4

f :: Integer -> Integer
f x = 3 * x^5 + 4 * x^3 + 7 * x^2 + 4

first = head [x | x <- [0..], f x > 1000]
first' = last [x | x <- [0..], f x <= 1000] + 1 -- Nem jó, mert a Haskell nem tudja, hogy 3 után sosem teljesül a feltétel, és megpróbálja felépíteni a végtelen listát
first'' = last [x | x <- [0..100], f x <= 1000] + 1

-- Azokat a számpárokat, amelyekre igaz, hogy az egyik szám önmagánál kisebb osztóinak összege a másik számmal egyenlő és fordítva, barátságos számoknak hívjuk. Adjuk meg azt a kifejezést, amely előállítja a 220 önmagánál kisebb egész osztóinak összegét! Adjuk meg ugyanezt a kifejezést a 284-re is!

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x `div` 2], x `mod` d == 0]

-- sum (divisiors 220)

-- Adjuk meg azt a logikai kifejezést, amely az a) pontban megadott kifejezések felhasználásával eldönti, hogy a 220 és a 284 barátságos számpárt alkot-e! Amennyiben barátságos számpárt alkotnak, úgy a kifejezés értéke legyen True, ellenkező esetben False.

areFriendly :: Int -> Int -> Bool
areFriendly x y = sum (divisors x) == y && x == sum (divisors y)

-- Állíts elő egy olyan rendezett párokból álló listát, amelyben szerepel az első 50 pozitív egész szám és a szám osztói!
first50WithDivisors :: [(Int, [Int])]
first50WithDivisors = [(x, divisors x ++ [x]) | x <- [1..50]]

-- Képezz le egy szöveget egy listára, amely azon sorok számait tartalmazza, amelyek üresek voltak! Tipp: Hoogle :)

-- empty :: String -> [Int]
empty xs = [nr | (nr, s) <- zip [1..] (lines xs), null s]


-- Definiálj egy függvényt, amely eldönti, hogy egy predikátum teljesül-e egy lista minden elemére!
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = null [1 | x <- xs, not (p x)]

all'' :: (a -> Bool) -> [a] -> Bool
all'' p xs = sum [if p x then 1 else 0 | x <- xs] == length xs

all''' :: (a -> Bool) -> [a] -> Bool
all''' p []     = True
all''' p (x:xs) = p x && all''' p xs





