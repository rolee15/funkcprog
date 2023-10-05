

-- ismetles:
inc x = x + 1

x = 0

f x = Main.x -- <- a fenti x = 0-ra hivatkozik

-- Mi lesz az eredmenye x-nek, ha lefuttatjuk 'inc x'-et?

-- nem lehet túlterhelni
-- inc a b = a + b

is0 :: Int -> Bool
is0 x = x == 0

is0' :: Int -> Bool
is0' 0 = True
is0' x = False

{-
        {  i   , ha x = 0
  f x = {  h   , ha x <> 0
        {
-}


is0'' :: Int -> Bool
is0'' x = False
-- is0'' 0 = True -- redundáns


is0''' :: Int -> Bool
is0''' 0 = True
is0''' _ = False

-- mintaillesztes es tobb egyenlettel megadott fuggvenyek
-- wildcard minta
-- A felsorolas sorrendje!
-- Add meg a logikai 'es'-t

not' :: Bool -> Bool
not' True  = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False

and'' :: Bool -> Bool -> Bool
and'' True  True   = True
and'' True  False  = False
and'' False True   = False
and'' False False  = False

{-
and''' :: Bool -> Bool -> Bool
and''' x x = True  -- "repeated pattern", nem lehet parametert ismetelni
and''' _ _ = False
-}

xor :: Bool -> Bool -> Bool
xor x y = x /= y   -- Bool-ra van == és /= a Prelude-ban

or' :: Bool -> Bool -> Bool
or' = undefined

-- osszeadas kettes szamrendszerben 2 biten:
add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (0, 0) x = x

add2' :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2' (x1, x2) (y1, y2) = ((x1 + y1 + div (x2 + y2) 2) `mod` 2, (x2 + y2) `mod` 2) -- HF jó-e?



-- Adj meg egy fuggenyt, amely a sortores karaktert szokozre "csereli"
swapEndline :: Char -> Char
swapEndline '\n' = ' '
swapEndline x    = x

-- szamologep: calc
calc :: Int -> Char -> Int -> Int
calc = undefined


-- osztast is tudo szamologep:
calc' :: Int -> Char -> Int -> Int -- Double
calc' x '+' y = x + y
calc' x '*' y = x * y
calc' x '/' y = x `div` y

calc'' :: Int -> Char -> Int -> Double
calc'' x '+' y = fromIntegral (x + y)
calc'' x '*' y = fromIntegral (x * y)
calc'' x '/' y = fromIntegral x / fromIntegral y


