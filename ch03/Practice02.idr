import Data.Vect

length1 : Vect n elem -> Nat
length1 [] = Z
length1 (x :: xs) = S (length1 xs)

length2 : Vect n elem -> Nat
length2 {n} xs = n

createEmpties : Vect n (Vect 0 a)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties

