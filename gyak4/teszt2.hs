xor :: Bool -> Bool -> Bool
xor True   True   = False
xor False  False  = False
xor _      _      = True

pairDiv :: (Double, Double) -> Double -> (Double, Double)
pairDiv (x, y) z = (x / z, y / z)