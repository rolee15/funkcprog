import Data.Char
import Numeric

unsafeLookup :: Eq a => a -> [(a,b)] -> b
unsafeLookup k []           = error "unsafeLookup: Key not found"
unsafeLookup k ((x1,x2):xs) = if k == x1 then x2
                              else unsafeLookup k xs

parseCharOfBase :: Integer -> Char -> Integer
parseCharOfBase b x
    | b < 2 || b > 16              = error ("parseCharOfBase: Invalid base " ++ show b)
    | toInteger(digitToInt x) >= b = error ("parseCharOfBase: Digit is invalid in given base " ++ show x)
    | otherwise                    = toInteger(digitToInt x)

parseStringOfBase = map . parseCharOfBase

parseInteger :: Int -> String -> Integer
parseInteger b x
    | b < 2 || b > 16 = error ("parseInteger: Invalid base " ++ show b)
    | x == ""         = error "parseInteger: Empty string"
    | otherwise       = sum (map (* toInteger b) (parseStringOfBase (toInteger b) x))

-- [a0, a1, a2, ..., ak], length: k+1
-- ak * 16^k + ... + a2 * 16^2 + a1 * 16^1 + a0 * 16^0
-- or
-- a0 * 16^0 + a1 * 16^1 + a2 * 16^2 + ... + ak * 16^k