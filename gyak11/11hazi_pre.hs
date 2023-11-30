{-# LANGUAGE InstanceSigs #-}
--              ^--- ez az opcio engedelyezi a fuggvenyek tipusainak megadasat tipusosztaly-peldanyokban 

import Data.List
import Data.Char
import Text.Read

data Time = T Int Int

showTime :: Time -> String
showTime (T x y) = show x ++ ":" ++ show y

instance Show Time where
  show :: Time -> String
  show = showTime

-- 1. feladat:
eqTime :: Time -> Time -> Bool
eqTime = undefined

instance Eq Time where
  (==) :: Time -> Time -> Bool
  (==) = eqTime

-- Megjegyzés, nem szükséges az `eqTime`, `isEarlier` függvényeket külön definiálni, rögtön meg lehet őket adni az `instance`-on belül

-- 2. feladat:
isEarlier :: Time -> Time -> Bool
isEarlier = undefined

instance Ord Time where
  (<=) :: Time -> Time -> Bool
  t1 <= t2 = isEarlier t1 t2 || t1 == t2

-- 3. feladat:
time :: Int -> Int -> Maybe Time
time = undefined

-- 4. feladat (ezt a definiciot csereld ki a valid Month definicioval):
data Month = DummyDefinition deriving (Eq, Ord, Show) -- Ha Ord derived, akkor fontos a honapok felsorolasanak sorrendje!!!

-- iLikeAutumn :: Month -> Bool
iLikeAutumn = undefined


-- Bonusz 1:
parseTime :: String -> Either Time String
parseTime = undefined

data Date = D { day :: Int, month :: Month, year :: Int, clock :: Time} deriving (Eq, Show)

-- Bonusz 2/a:
instance Ord Date where
  (<=) :: Date -> Date -> Bool
  (<=) = undefined

-- Bonusz 2/b:
validAssignments :: Date -> [(Date, String)] -> [String]
validAssignments = undefined

