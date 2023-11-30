polinom :: Num a => [a] -> a -> a
polinom xs k = foldr (\x acc -> x + acc * k) 0 xs