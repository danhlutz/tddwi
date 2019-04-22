-- do section 8.3 exercises again for practice
data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : (x : a) -> (Vect k a) -> (Vect (S k) a)

-- 1
headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
              (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl


tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
              (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

-- 2
decConsEq : DecEq a => {xs : Vect k a} -> {ys : Vect k a} ->
            (headEq : x = y) -> (tailEq : xs = ys) -> (x :: xs) = (y :: ys)
decConsEq Refl Refl = Refl


DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) =
    case decEq x y of
         No contraHead => No (headUnequal contraHead)
         Yes headEq => case decEq xs ys of
                            (Yes tailEq) => Yes (decConsEq headEq tailEq)
                            (No contraTail) => No (tailUnequal contraTail)
