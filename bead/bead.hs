import Data.Char
import Data.List
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
    | all isSpace x   = error "parseInteger: Empty string"
    | otherwise       = foldl' (\acc digit -> acc * toInteger b + toInteger digit) 0 (parseStringOfBase (toInteger b) x)

-- [a0, a1, a2, ..., ak], length: k+1, base: b
-- ak * b^k + ... + a2 * b^2 + a1 * b^1 + a0 * b^0
-- or
-- a0 * b^0 + a1 * b^1 + a2 * b^2 + ... + ak * b^k
-- can be reoreder to
-- a0 + b(a1 + b(a2 + ... (ak-1 + b * ak)))
-- e.g.
-- 12 + 10 * 16 + 15 * 16^2 + 0 * 16^3

hexToDecimal :: [Int] -> Integer
hexToDecimal = foldl' (\acc digit -> acc * 16 + toInteger digit) 0