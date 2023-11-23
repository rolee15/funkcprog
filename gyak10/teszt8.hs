import Data.Char

allToUpper :: String -> String
allToUpper = map toUpper

{-this function filters the strings from a list strings that end with the letter 'a' -}
aWords :: [String] -> [String]
aWords [] = []
aWords xs = filter (\x -> if length x == 0 then False else last x == 'a') xs