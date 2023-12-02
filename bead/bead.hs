import Data.Char
import Numeric

unsafeLookup :: Eq a => a -> [(a,b)] -> b
unsafeLookup k []           = error "unsafeLookup: Key not found"
unsafeLookup k ((x1,x2):xs) = if k == x1 then x2
                              else unsafeLookup k xs

parseCharOfBase :: Integer -> Char -> Integer
parseCharOfBase b x
    | b < 2 || b > 16              = error ("parseCharOfBase: Invalid base " ++ show b)
    | toInteger(digitToInt x) >= b = error ("parseCharOfBase: Digit is invalid in given base: " ++ show x)
    | otherwise                    = toInteger(digitToInt x)

parseInteger :: Int -> String -> Integer
parseInteger = undefined