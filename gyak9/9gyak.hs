import Data.Char
import Data.List

hasAny :: Eq a => [a] -> [a] -> Bool
hasAny xs ys = not $ null $ filter (\x -> x `elem` ys) xs 

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

mymap :: (a -> b) -> [a] -> [b]
mymap f []     = []
mymap f (x:xs) = f x : mymap f xs

double' = map (\x -> if even x then 2 * x else x)

toString' = map chr

double'' :: [Int] -> [Int]
double'' = map (*2) . filter even --xs = map (*2) (filter even xs)

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

-- Valogass szet egy listat az elemek paritasa szerint!
oddEven :: [Int] -> ([Int], [Int])
oddEven [] = ([], [])
oddEven (x:xs) = case oddEven xs of
                    (ys, zs) -> if even x
                                then (x:ys, zs)
                                else (ys, x:zs)

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f d []     = d
myfoldr f d (x:xs) = f x (myfoldr f d xs)

summ' xs = foldr (+) 0 xs

sumEvens' xs = foldr (\x acc -> acc + if even x then x else 0) 0 xs

and' xs = foldr (&&) True xs

oddEven' xs = foldr (\x acc -> case acc of
                                  (ys, zs) -> if even x
                                              then (x:ys, zs)
                                              else (ys, x:zs)) ([], []) xs

oddEven'' xs = foldr (\x (ys, zs) -> if even x
                                     then (x:ys, zs)
                                     else (ys, x:zs)) ([], []) xs

takewhile' = undefined

dropwhile' = undefined

-- foldr1

-- tail recursion, foldl', rekurzio egyeb tipusokon (pl szamok)
myfoldl = undefined

-- $ operatoros modon felirt fact:
fact :: Integer -> Integer
fact 0 = 1
fact n = (n *) $ fact $ n - 1

fact2 :: Integer -> Integer -> Integer
fact2 = undefined

fib :: Integer -> Integer
fib = undefined

fib2 :: Integer -> Integer -> Integer -> Integer
fib2 = undefined


