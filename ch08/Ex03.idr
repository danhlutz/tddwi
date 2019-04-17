same_cons : {xs : List a} -> {ys : List a} ->
            xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

same_list : {xs : List a} -> {ys : List a} ->
            x = y -> xs = ys -> x :: xs = y :: ys
same_list Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  SameThree : ThreeEq val val val

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x SameThree = SameThree
