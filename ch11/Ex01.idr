-- 11.1 exercises
import Data.Primitives.Views
-- 1
every_other : Stream a -> Stream a
every_other (_ :: val :: rest) = val :: every_other rest

-- 2
data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

Functor InfList where
  map func (x :: xs) = (func x) :: map func xs

countFrom : (start : Nat) -> InfList Nat
countFrom x = x :: countFrom (S x)

getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z x = []
getPrefix (S k) (x :: xs) = x :: getPrefix k xs

-- 3
data Face = Heads | Tails

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

getFace : Int -> Face
getFace num with (divides num 2)
  getFace ((2 * div) + rem) | (DivBy prf) =
          case rem of
               0 => Heads
               _ => Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z xs = []
coinFlips (S k) (x :: xs) = getFace x :: coinFlips k xs

-- 4
square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = 
  let next = (approx + (number / approx)) / 2 in
      approx :: square_root_approx number next

-- 5
square : Num a => a -> a
square x = x * x

square_root_bound : (max : Nat) -> (number: Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Double
square_root_bound Z number bound (a :: as) = a
square_root_bound (S k) number bound (a :: as) = 
                  let squared = square a in
                  case abs (number - squared) < bound of
                       True => a
                       False => square_root_bound k number bound as

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
                                 (square_root_approx number number)
