import Data.Char
import Data.List
import Numeric
import Distribution.Compat.Lens (_1)
import Control.Exception (evaluate)

unsafeLookup :: (Eq a) => a -> [(a, b)] -> b
unsafeLookup k [] = error "unsafeLookup: Key not found"
unsafeLookup k ((x1, x2) : xs) =
  if k == x1
    then x2
    else unsafeLookup k xs

parseCharOfBase :: Integer -> Char -> Integer
parseCharOfBase b x
  | b < 2 || b > 16 = error ("parseCharOfBase: Invalid base " ++ show b)
  | toInteger (digitToInt x) >= b = error ("parseCharOfBase: Digit is invalid in given base " ++ show x)
  | otherwise = toInteger (digitToInt x)

parseStringOfBase = map . parseCharOfBase

-- [a0, a1, a2, ..., ak], length: k+1, base: b
-- ak * b^k + ... + a2 * b^2 + a1 * b^1 + a0 * b^0
-- or
-- a0 * b^0 + a1 * b^1 + a2 * b^2 + ... + ak * b^k
-- can be reoreder to
-- a0 + b(a1 + b(a2 + ... (ak-1 + b * ak)))
-- e.g.
-- 12 * 16^7 + 10 * 16^6 + 15 * 16^5 + 14 * 16^4 + 11 * 16^3 + 10 * 16^2 + 11 * 16 + 14
-- ((((((12 * 16 + 10) * 16 + 15) * 16 + 14) * 16 + 11) * 16 + 10) * 16 + 11) * 16 + 14
parseInteger :: Int -> String -> Integer
parseInteger b x
  | b < 2 || b > 16 = error ("parseInteger: Invalid base " ++ show b)
  | all isSpace x = error "parseInteger: Empty string"
  | otherwise = foldl' (\acc digit -> acc * toInteger b + toInteger digit) 0 (parseStringOfBase (toInteger b) x)

parseLiteral :: String -> Integer
parseLiteral s
  | all isSpace s = error "parseLiteral: Empty string"
  | take 2 s == "0x" || take 2 s == "0X" = parseInteger 16 (drop 2 s)
  | take 2 s == "0o" || take 2 s == "0O" = parseInteger 8 (drop 2 s)
  | take 2 s == "0b" || take 2 s == "0B" = parseInteger 2 (drop 2 s)
  | otherwise = parseInteger 10 s

isOperator :: Char -> Bool
isOperator x = x `elem` ['(', ')', '+', '-', '~', '/', '%', '*']

type Token = String

tokenize :: String -> [Token]
tokenize "" = []
tokenize str
  | isSpace (head str) = tokenize (tail str)
  | isOperator (head str) = [head str] : tokenize (tail str)
  | otherwise = takeWhile (\c -> not (isSpace c) && not (isOperator c)) str : tokenize (dropWhile (\c -> not (isSpace c) && not (isOperator c)) str)

precedence :: Token -> Int
precedence "+" = 1
precedence "-" = 1
precedence "/" = 2
precedence "*" = 2
precedence "%" = 2
precedence "~" = 3


isOperatorString :: String -> Bool
isOperatorString xs = length xs == 1 && isOperator (head xs)

shuntStep ::[Token] -> Token -> ([Token], [Token])
shuntStep stack token
  | token == "(" = (token : stack, [])
  | token == ")" = let (tokens, newStack) = break (== "(") stack in (drop 1 newStack, tokens)
  | isOperatorString token && (null stack || head stack == "(" || precedence token > precedence (head stack)) = (token : stack, [])
  | isOperatorString token = let (tokens, newStack) = span (\x -> x =="(" && precedence token <= precedence x) stack in (token : newStack, tokens)
  | otherwise = (stack, [token])

shunt :: [Token] -> [Token] -> [Token]
shunt stack [] = stack
shunt stack (x : xs) = let (newStack, tokens) = shuntStep stack x in tokens ++ shunt newStack xs


unaryToOp :: Token -> (Integer -> Integer)
unaryToOp "~" = negate

binaryToOp :: Token -> (Integer -> Integer -> Integer)
binaryToOp "+" = (+)
binaryToOp "-" = (-)
binaryToOp "*" = (*)
binaryToOp "/" = div
binaryToOp "%" = mod

calculateStep :: [Integer] -> Token -> [Integer]
calculateStep stack token
  | token == "~"           = case stack of
                              (x:xs) -> unaryToOp "~" x : xs
                              _ -> error "calculateStep: not enough parameters when applying ~"
  | isOperatorString token = case stack of
                              (x:y:xs) -> binaryToOp token y x : xs
                              _ -> error ("calculateStep: not enough parameters when applying " ++ token)
  | otherwise              = parseLiteral token : stack

calculate :: [Integer] -> [Token] -> Integer
calculate stack [] = head stack
calculate stack (x:xs) = calculate (calculateStep stack x) xs

evaluateExpression :: String -> Integer
evaluateExpression = calculate [] . shunt [] . tokenize