
||| Represents Shapes
data Shape = ||| A triangle, with its base and height
             Triangle Double Double
           | ||| A rectangle, with its length and width
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

% name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle b h) = 0.5 * b * h
area (Rectangle l w) = l * w
area (Circle r) = pi * r * r

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

% name Picture pic, pic1, pic2

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

triangle2 : Picture
triangle2 = Primitive (Triangle 1 1)

testPicture : Picture
testPicture = Combine (Translate 3 2 triangle2)
              (Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle)))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = (pictureArea pic) + (pictureArea pic1)
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

bigTriangleHelper : (shape : Shape) -> Biggest
bigTriangleHelper shape = case shape of
                               (Triangle _ _) => Size (area shape)
                               _ => NoTriangle

compareHelper : (big : Biggest) -> (big1 : Biggest) -> Biggest
compareHelper NoTriangle big1 = big1
compareHelper big NoTriangle = big
compareHelper (Size x) (Size y) =
  if x > y then Size x else Size y

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive shape) = bigTriangleHelper shape
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
biggestTriangle (Combine pic pic1) =
  let big = biggestTriangle pic
      big1 = biggestTriangle pic1 in
  compareHelper big big1
