import Data.Char
import Data.List

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f d []     = d
foldr f d (x:xs) = f x (foldr f d xs)
-}
{-
  foldr f d [e_1, e_2, e_3, ..., e_n] = e_1 `f` (e_2 `f` (e_3 `f` ... (e_n `f` d) ... ))
-}

{-
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
-}

length' :: [a] -> Int
length' = undefined


{-
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
-}

mapfoldr :: (a -> b) -> [a] -> [b]
mapfoldr = undefined

{-
filter :: (a -> Bool) -> [a] -> [a]
filter p []     = []
filter p (x:xs) = if p x
                  then x : filter p xs
                  else     filter p xs
-}

filterfoldr :: (a -> Bool) -> [a] -> [a]
filterfoldr = undefined

-- rekurzívan is!
any' :: (a -> Bool) -> [a] -> Bool
any' = undefined

all' :: (a -> Bool) -> [a] -> Bool
all' = undefined

-- rekurzívan is!
takewhile' :: (a -> Bool) -> [a] -> [a]
takewhile' = undefined

-- add meg az első szót egy szövegből!

{-
takeWord "" == ""
takeWord " fa" == ""
takeWord "alma fa" == "alma"
takeWord "almafa" == "almafa"
-}

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl = undefined

{-
  foldl f d [e_1, e_2, e_3, ..., e_n] =  ( ... (((d `f` e_1) `f` e_2) `f` e_3) ... `f` e_n)
-}

-- rekurzívan is!
reverse' :: [a] -> [a]
reverse' = undefined

-- foldr?
reverse'' :: [a] -> [a]
reverse'' = undefined

-- foldl?
reverse''' :: [a] -> [a]
reverse''' = undefined

-- tail recursion:
fact :: Integer -> Integer
fact n
  | n == 0 = 1
  | n > 0  = n * fact (n - 1)

fact2 :: Integer -> Integer -> Integer
fact2 n m = undefined

-- forall n: fact n == fact n 1

-- foldl is tail-recursive, foldr is not
