
-----------------
-- UJRA LISTAK --
-----------------

-- hany fajta listat ismersz?

head' :: [a] -> a
head' (x:xs) = x
-- head' []     = ?

tail' = undefined


-- ures-e egy lista?
null' :: [a] -> Bool
null' [] = True
null' _  = False

-- egyelemu-e egy lista?
isSingletonList :: [a] -> Bool
isSingletonList xs = null (tail xs)

isSingletonList' :: [a] -> Bool
isSingletonList' xs = length xs == 1

isSingletonList'' :: [a] -> Bool
isSingletonList'' (x:[]) = True  -- [x]
isSingletonList'' _      = False


-- ketelemu-e egy lista?
-- x:y:[]


-- stb. Hogyan lehetne megirni azt, hogy 139 elemu-e egy lista?
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

-- take, drop - HF

{-
-- rekurzio: nezzuk meg egyesevel kibontva a beta-redukciokat!
  length [1,2,3] = length (1:[2,3]) =>
  1 + length [2,3] = 1 + length (2:[3]) =>
  1 + (1 + length [3]) = 1 + (1 + length (3:[])) =>
  1 + (1 + (1 + length [])) => 1 + (1 + (1 + 0)) => 3
-}

-- szorozd ossze a lista elemeit!
prod :: Num a => [a] -> a
prod []     = 1
prod (x:xs) = x * prod xs

prod' :: Num a => [a] -> a
prod' []     = error "Empty list"
prod' [x]    = x
prod' (x:xs) = x * prod xs

-- Add meg egy lista minden 3. elemet!
everyThird :: [a] -> [a]
everyThird (_:_:x:xs) = x : everyThird xs
everyThird _ = []

-- Add meg egy lista n-edik elemet! (error fuggveny)
nth :: [a] -> Int -> a
nth = undefined -- HF

-- Fuzz ossze ket listat! (zip fuggveny)
zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' _      _      = []

-- Add meg az osszefesulo rendezes osszefesulo lepeset (merge fuggveny)!
-- Ebben a lepesben ket listat kell rendezetten osszefesulni.
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x <= y      = x : merge xs (y:ys)
  | otherwise   = y : merge (x:xs) ys
merge []     ys = ys
merge xs     [] = xs

{-

merge [3,9,10] [4,5,11] =>
3 : merge [9,10] [4,5,11] =>
3 : 4 : merge [9,10] [5,11] => ...

-}

-- HF: mergeSort az előző függvénnyel
-- take + drop




















