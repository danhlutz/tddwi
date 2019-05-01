import Shape_abs

area : Shape -> Double
area x with (shapeView x)
  area (triangle base width) | STriangle = 0.5 * base * width
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = pi * radius * radius


