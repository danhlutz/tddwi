-- section 8.3 exercises

data Vect : Nat -> Type -> Type where
     Nil  : Vect 0 a
     (::) : (x : a) -> (Vect k a) -> (Vect (S k) a)

-- 1
headUnequal : DecEq a => {xs : Vect n a} -> {ys: Vect n a} ->
              (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl


tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
              (contra : (xs = ys) -> Void) ->
              ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

-- 2
decEqCons : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
            (headEqual : x = y) -> (tailEqual : xs = ys) ->
            (x :: xs) = (y :: ys)
decEqCons Refl Refl = Refl

DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) =
    case decEq x y of
         No contra => No (headUnequal contra)
         Yes headEqual => 
             case decEq xs ys of
                  (Yes tailEqual) => Yes (decEqCons headEqual tailEqual)
                  (No contra) => No (tailUnequal contra)
