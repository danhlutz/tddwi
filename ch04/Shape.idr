||| Represents Shapes
data Shape = ||| A triangle, with its base and height
             Triangle Double Double
           | ||| A rectangle, with its length and width
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

area : Shape -> Double
area (Triangle b h) = 0.5 * b * h
area (Rectangle l w) = l * w
area (Circle r) = pi * r
