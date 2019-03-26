safeDivide : Double -> Double -> Maybe Double
safeDivide x 0.0 = Nothing
safeDivide x y = Just (x / y)
