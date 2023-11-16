-- Teszt 7
--

hasAny :: Eq a => [a] -> [a] -> Bool 
hasAny xs ys = length [ x | x <- xs, x `elem` ys] > 0
