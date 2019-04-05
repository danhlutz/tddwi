-- Section 6.2 exercises

import Data.Vect

--1
Matrix : (m : Nat) -> (n : Nat) -> Type
Matrix m n = Vect m (Vect n Double)

testMatrix : Matrix 2 3
testMatrix = [[0,0,0], [1,2,3]]
