data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : (x : a) -> (xs : Vect n a) -> Vect (S n) a

data Elem : a -> Vect k a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInXs : (notThere : Elem value xs -> Void) ->
          (notHere : (value = x) -> Void) ->
          Elem value (x :: xs) -> Void
notInXs notThere notHere Here = notHere Refl
notInXs notThere notHere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) =
       case decEq value x of
            (Yes Refl) => Yes Here
            (No notHere) => case isElem value xs of
                                 (Yes later) => Yes (There later) 
                                 (No notThere) => No (notInXs notThere notHere)
