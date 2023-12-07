{-# LANGUAGE InstanceSigs #-}

-- 2 elemu tipus? -- Bool
-- 1 elemu tipus? -- (), unit, Top, "True"
-- 0 elemu tipus? -- undefined mas nyelvekben: "False", Bottom, Empty

-- 2. Parameteres adatkonstruktorok:
-- Time

data Time = T Int Int

instance Show Time where  -- tipusosztaly peldanya
  show :: Time -> String
  show (T x y) = show x ++ ":" ++ show y

-- Tipusosztalyok: ad-hoc polimorfizmust tesznek lehetove:
-- Egy adott fuggveny tetszoleges ertekre alkalmazhato, _amelynek_ van a megadott tipusosztalyhoz peldanya
-- pl. :t show => show :: Show a => a -> String
-- Ennek a modulnak a betoltese utan Time-ra is alkalmazhato a show, de mondjuk (Int -> Int)-re nem.
-- peldanyok listazasa: :i <tipusosztaly>

instance Eq Time where
  (==) :: Time -> Time -> Bool
  (==) = undefined

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

-----------------------------
-- Parameteres adattipusok --
-----------------------------
-- data Maybe a = Nothing | Just a deriving (Eq, Show)

-- A Maybe onmagaban nem egy tipus! Adott "a" tipus eseten a "Maybe a" mar egy konkret tipus.
-- A Maybe egy tipuskonstruktor, ami parameteres (olyan, mint egy fuggveny). A tipusok tipusa Haskellben: kind
-- :k Maybe => Maybe :: * -> *
-- :k Time => Time :: *
-- :k [] => [] :: * -> *    -- <- ez a lista tipuskonstruktora, ugyanaz, mint az ures lista ertek!
                            -- A tipuskonstruktor es az adatkonstruktor neve lehet ugyanaz

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
  | y == 0    = Nothing
  | otherwise = Just $ x `div` y

-- Keszitsunk egy map-ot: kulcs-ertek parok listaja. A kulcsok legyenek Intek, ertekek tetszoleges tipusuak.
-- Legyen put, get muvelet (egyszeruseg kedveert csak (==)-t vizsgaljunk, HF: rendezetten megvalositott map)
type Map a = [(Int, a)]

put :: Int -> a -> Map a -> Map a
put k v []     = [(k, v)]
put k v (x:xs) = if fst x /= k then x:put k v xs else (k, v):xs

get :: Int -> Map a -> Maybe a
get k []     = Nothing
get k ((kx, vx):xs)
  | k == kx   = Just vx
  | otherwise = get k xs

-- 3. b) Either
-- osszeg tipus:
-- data Either a b = Left a | Right b deriving (Eq, Show)

-- Adj meg egy Int-eket es String-eket tartalmazo mapot!
exampleMap :: Map (Either Int String)
exampleMap = [(1, Left 2), (2, Right "hello"), (3, Left 4)]

-- izomorfizmus ujra:
-- Mutasd meg, hogy az Either a () tipus izomorf a Maybe a tipussal
f :: Maybe (Either a b) -> Either a (Maybe b)
f Nothing          = Right Nothing
f (Just (Left x))  = Left x
f (Just (Right x)) = Right (Just x)

g :: Either a (Maybe b) -> Maybe (Either a b)
g (Left x)         = Just (Left x)
g (Right Nothing)  = Nothing
g (Right (Just x)) = Just (Right x)


data Shape2D = Circle Double               -- sugar
             | Rectangle Double Double     -- oldalak hossza
  deriving Show
type Shape3D = (Shape2D, Double) -- alap + magassag

draw :: Shape2D -> Double -> Either Shape2D Shape3D
draw shape height
  | height <= 0 = Left shape
  | otherwise   = Right (shape, height)

evenMaybe :: Maybe Int -> Maybe Bool
evenMaybe Nothing  = Nothing
evenMaybe (Just x) = Just $ even x

-------------------------------
-- Rekurziv adattipusok: fak --
-------------------------------

-- Emlekezteto: listak is rekurziv ADT-k

data MyList a = Nil | Cons a (MyList a) deriving Show

myHead :: MyList a -> Maybe a
myHead Nil = Nothing        -- []
myHead (Cons x xs) = Just x -- x:xs => (:) x xs

-- Fa tipus, amely leveleiben es csucsaiban is tartalmaz ertekeket
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

-- data Tree a = Leaf   | Node (Tree a) a (Tree a) -- <- csak a csucsokban tarol erteket
-- data Tree a = Leaf a | Node (Tree a)   (Tree a) -- <- csak a levelekben tarol erteket
-- data Tree a b = Leaf a | Node (Tree a b) b (Tree a b) -- <- csucsokban `b`, levelekben `a` tipusu ertekeket tarol

myTree = Node (Node (Leaf 1) 3 (Leaf 2)) 7 (Node (Leaf 4) 6 (Leaf 5))
{-
         7
       /   \
      3     6
     / \   / \
    1   2 4   5
-}

myTree2 = Node (Node (Leaf 2) 4 (Leaf 3)) 10 (Leaf 2)
{-
         10
       /   \
      4     2
     / \
    2   3
-}

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l _ r) = 1 + (height l `max` height r)

-- transzformalj egy Int-eket tartalmazo listat ugy, hogy minden tartolt erteket megduplazol!
doubleTree :: Tree Int -> Tree Int
doubleTree = undefined

-- Functor tipusosztaly: tetszoleges kontener adattipus elemeit lehet transzformalni
--   Korabban vettuk: map fuggveny listakra
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = undefined

-- add meg a doubleTree fuggvenyt fmap segitsegevel!
doubleTree' = undefined

-- Elozo oran vettuk a kovetkezo ket fuggvenyt:
sumTree :: Num a => Tree a -> a
sumTree (Leaf x)     = x
sumTree (Node l x r) = sumTree l + x + sumTree r

makeList :: Tree a -> [a]
makeList (Leaf x)     = [x]
makeList (Node l x r) = makeList l ++ [x] ++ makeList r

myStringTree = Node (Node (Leaf "This ") "is " (Leaf "an ")) "inorder " (Leaf "traversal")

-- Adj meg egy fuggvenyt, ami konkatenalja a faban tarolt listakat
concatTree :: Tree [a] -> [a]
concatTree = undefined

-- Hajtogatas tetszoleges kontener tipusra: Foldable tipusosztaly
--   Korabban vettuk: foldr fuggveny listakra
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = undefined

-- Miert jo ezt megadni a sajat fa tipusunkra?
-- Nezzuk meg, milyen fuggvenyeket tartalmaz a Foldable:
--   :i Foldable


-- Hogyan lehetne a korabbi fuggenyeket megadni a Foldable peldany segitsegevel? Egyaltalan meg kell adni oket?
concatTree' :: Foldable t => t [a] -> [a]
concatTree' = undefined

