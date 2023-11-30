
import Prelude hiding (Maybe, Either, Left, Right, Nothing, Just, foldl)  -- mai oran vett anyagokat elrejtjuk a Prelude modulbol

-- Kiegeszites: vegrekurzio/tail-recursion, foldr, foldl, foldl', foldr1, foldl1 hasznalata

-- tail recursion:
fact :: Integer -> Integer
fact n
  | n <= 0 = 1
  | n > 0  = n * fact (n - 1)

fact2 :: Integer -> Integer -> Integer
fact2 n m
  | n <= 0 = m
  | n > 0  = fact2 (n - 1) (n * m)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f d [] = d
foldl f d (x:xs) = foldl f (f d x) xs

-- foldl vegrekurziv, emiatt hatekonyabb lehet bizonyos esetekben, viszont sosem terminal vegtelen listak eseten!
-- foldl' : foldl moho valtozata, az `f d x` kifejezest moho modon ertekeli ki, emiatt joval hatekonyabb lehet, mint a foldl/foldr sok esetben
-- foldl1/foldr1 : foldl/foldr variaciok, nemures listakra hasznalhatoak, es a foldl1 az elso listaelemet, a foldr1 az utolso listaelemet
-- tekinti a hajtogatas alapertekenek


----------------------------------------------
-- Algebrai adattipusok, deriving, Eq, Show --
----------------------------------------------

-- 1. Felsorolasi tipusok

-- Dances

data Dance = Social | Salsa | WestCoast | Bachata deriving (Show)
data Sex = Male | Female deriving (Show)

isSocial :: Dance -> Bool
isSocial = undefined

dancers :: [(Dance, [(String, String, Bool, Sex)])]
dancers = [(Salsa, [("Kiss", "Samuel", True, Male),
                      ("Kiralyi", "Evelin", True, Female),
                      ("Kovacs", "Bela", False, Male),
                      ("Nemeth", "Csilla", True, Female),
                      ("Solymos", "Klara", True, Female),
                      ("Mars", "Bruno", False, Male),
                      ("Kovacs", "Agoston", False, Male),
                      ("Nemeth", "Balazs", True, Male)]),
           (Social,
                      [("Nagy", "Salamon", False, Male),
                       ("Janosi", "Janos", False, Male),
                       ("Elek", "Tibor", False, Male),
                       ("Nemeth", "Csilla", True, Female),
                       ("Nagy", "Xenia", True, Female),
                       ("Musk", "Elon", True, Male),
                       ("Solymos", "Klara", True, Female),
                       ("Nemeth", "Balazs", False, Male)]),
           (WestCoast,
                      [("Kis", "Sandor", True, Male),
                       ("Moricz", "Zsigmond", True, Male),
                       ("Elek", "Tibor", True, Male),
                       ("Nemeth", "Csilla", True, Female),
                       ("Nagy", "Sandor", True, Male),
                       ("Musk", "Elon", True, Male),
                       ("Solymos", "Klara", True, Female),
                       ("Nemeth", "Balazs", True, Male)])
          ]

-- Add meg azokat a noket, akik tarsastancban keresnek partnert!
socialWomen :: [(Dance, [(String, String, Bool, Sex)])] -> [String]
socialWomen xs = undefined

-- 2 elemu tipus?
-- 1 elemu tipus?
-- 0 elemu tipus?

-- 2. Parameteres adatkonstruktorok:
-- Time ~ szorzat tipus

data Time = T Int Int

instance Show Time where  -- tipusosztaly peldanya
  show :: Time -> String
  show = undefined

instance Eq Time where
  (==) :: Time -> Time -> Bool
  (==) = undefined

-- izomorf a (Int, Int) tipussal, azaz meg tudunk adni olyan 
--   f :: Time -> (Int, Int) es g :: (Int, Int) -> Time fuggvenyeket, 
--   hogy minden x::(Int, Int)-re: (f . g) x == x es y::Time-ra (g . f) y == y

fTime :: Time -> (Int, Int)
fTime = undefined

gTime :: (Int, Int) -> Time
gTime = undefined

-- Izomorfizmus tesztek:
input1 :: [(Int, Int)]
input1 = zip [0..23] [0..59]
input2 :: [Time]
input2 = zipWith T [0..23] [0..59]

test1 = input1 == map (fTime . gTime) input1
test2 = input2 == map (gTime . fTime) input2

-- MJ: ha a Haskellnel egy meg "szigorubb" tipusozasu nyelvet hasznalnank (pl. Coq, Agda), akkor ezt
--     nem tesztelnenk, hanem bizonyitanank tetszoleges inputra

validTime :: Time -> Bool
validTime = undefined

-- 3. Tipusparameteres adattipusok
-- 3. a) Maybe

data Maybe a = Nothing | Just a deriving (Eq, Show)

safeDiv :: Int -> Int -> Maybe Int
safeDiv = undefined

-- Keszitsunk egy map-ot: kulcs-ertek parok listaja. A kulcsok legyenek Intek, ertekek tetszoleges tipusuak.
-- Legyen put, get muvelet (egyszeruseg kedveert csak (==)-t vizsgaljunk, HF: rendezetten megvalositott map)
type Map a = [(Int, a)]

put :: Int -> a -> Map a -> Map a
put k v []     = [(k, v)]
put k v (x:xs) = if fst x /= k then x:put k v xs else (k, v):xs

get :: Int -> Map a -> Maybe a
get = undefined

-- 3. b) Either
-- osszeg tipus:
data Either a b = Left a | Right b deriving (Eq, Show)

-- Adj meg egy Int-eket es String-eket tartalmazo mapot!
-- exampleMap :: ?
-- exampleMap = undefined

-- izomorfizmus ujra:
-- Mutasd meg, hogy az Either a () tipus izomorf a Maybe a tipussal
f :: Maybe a -> Either a ()
f = undefined

g :: Either a () -> Maybe a
g = undefined


data Shape2D = Circle Double               -- sugar
             | Rectangle Double Double     -- oldalak hossza
  deriving Show
type Shape3D = (Shape2D, Double) -- alap + magassag

draw :: Shape2D -> Double -> Either Shape2D Shape3D
draw = undefined

