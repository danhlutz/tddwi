-- Section 8.1 exercises
-- 1
total
same_cons : {xs : List a} -> {ys : List a} ->
            xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

-- 2
total
same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl prf = cong prf

-- 3
data ThreeEq : a -> b -> c -> Type where
     ThreeSame : (n : Nat) -> ThreeEq n n n

-- 4
total
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z (ThreeSame z) = ThreeSame (S z)
