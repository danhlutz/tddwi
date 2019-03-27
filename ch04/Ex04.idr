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

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

primitiveHelper : (shape : Shape) -> Maybe Double
primitiveHelper s@(Triangle x y) = Just (area s)
primitiveHelper _ = Nothing

compareSizes : (size : Maybe Double) -> (size1 : Maybe Double) -> Maybe Double
compareSizes size Nothing = size
compareSizes Nothing size1 = size1
compareSizes (Just x) (Just y) = if x > y then Just x else Just y

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape) = primitiveHelper shape
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
biggestTriangle (Combine pic pic1) =
  let size = biggestTriangle pic
      size1 = biggestTriangle pic1 in
  compareSizes size size1
