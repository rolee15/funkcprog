module Asd where

putTuple :: forall a. a -> (a, a)
putTuple x = (,) x x 

sumTuple :: forall a. Ord a => a -> (a, Bool)
sumTuple x = (x, x == x)

-- data, type, newtype, class

data Days = Mon | Tue | Wed | Sat | Sun 
    deriving (Eq, Ord)

instance Show Days where
    show Mon = "Hetfo"
    show Tue = "Kedd"
    show Wed = "Szerda"
    

---instance Show Days where

data Domino = D (Int, Int)

--asd :: Domino 

--sumDomino :: Domino -> Int



headL :: List a -> Maybe a
headL Nil = Nothing
headL (Cons a _) = Just a

--data MaybeMy a = Nothing | Just a 
-- tipusok szintjen +1

 
inc :: Int -> Int
--inc a = a + 1
--inc a = 1 + a
--inc a = (1 +) a
inc = (1 +)

--inc :: Bool -> Int
--inc = undefined
-- (+) :: (Int -> (Int -> Int))


data Zero -- Bottom
data Bottom

data One = One -- data Top = Triv 

--ketelmu, Bool

-- Either
data Sum a b = Inl a | Inr b --- Int, Int => Inl 1, Inr 1   

type EqN a b = (a -> b, b -> a)

type Not a = a -> Bottom

type Decide a = Sum a (Not a)

type And a b = Prod a b 

type Or a b = Sum a b 

exfalso :: Bottom -> a
exfalso b = case b of 


deMorgan1 :: EqN (Not (And a b)) (And (Not a) (Not b))  

-- (,)
data Prod a b = Pair a b

data Exp a b = Fun (b -> a) -- Bool, Int == Bool^Int

-- Exp Bool Bool == Bool -> Bool, id, not, (const True), (const False)


-- Sum One One == Bool == Maybe One == Prod Bool One == Maybe (Maybe Zero)

data BinTree a = LeafB | NodeB a (BinTree a) (BinTree a)

data RoseTree a = LeafR | NodeR a [RoseTree a]

data List a = Nil | Cons a (List a)

data MaybeMy a = NothingM | JustM a 

data InfiniTree k a = LeafI | NodeI a (k -> InfiniTree k a)

type BinTree' a = InfiniTree Bool a

type RoseTree' a = InfiniTree Int a

--                        ()
type List' a = InfiniTree One a

type Maybe' a = InfiniTree Zero a

-- a^b * a^c <=> a^(b+c)
--lawExp :: Prod (Exp a b) (Exp a c) -> Exp a (Sum b c)
--lawExp = g where
--    g = 


leaf :: k -> InfiniTree k a
leaf = const LeafI
--    \ x -> Leaf  

maybeOne :: Maybe' Int -- Just 1
maybeOne = NodeI 1 leaf
--         NodeI 1 (\ k -> Leaf)


nestedIx :: InfiniTree k a -> [k] -> Maybe a
nestedIx LeafI _            = Nothing
nestedIx (NodeI x _) []     = Just x
nestedIx (NodeI _ f) (k:ks) = nestedIx (f k) ks

one :: Maybe Int
one = nestedIx maybeOne []




-- [1,2,3]
oneTwoThree :: List' Int
oneTwoThree = NodeI 1 (\ _ -> NodeI 2 (\ _ -> NodeI 3 leaf))

three :: Maybe Int
three = nestedIx oneTwoThree [One, One]



map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]


map'' f xs = do
    x <- xs
    return f x

map'' f xs = xs >>= (x -> f x)


h :: [a] -> (a -> [b]) -> [b]
h xs f = concat (map f xs)
--        [[b]]

-- flatMap, concatMap, bind

main :: IO ()
main = do 
    x <- readLine
    return x 

($) :: (a -> b) -> (a -> b)
($) = id

