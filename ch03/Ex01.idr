-- section 3.2 exercises
import Data.Vect

-- 1
my_length : List a -> Nat
my_length [] = Z
my_length (x :: xs) = S (my_length xs)

-- 2
my_reverse : List a -> List a
my_reverse xs = iter [] xs
  where iter : List a -> List a -> List a
        iter ans [] = ans
        iter ans (y :: ys) = iter (y :: ans) ys

-- 3
my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = (f x) :: (my_map f xs)

-- 4
my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs
