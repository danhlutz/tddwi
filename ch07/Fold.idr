sumLength : String -> Nat -> Nat
sumLength x k = (length x) + k

totalLen : List String -> Nat
totalLen xs = foldr sumLength Z xs
