data Vect : Nat -> Type -> Type where
     Nil  : Vect 0 a
     (::) : (x : a) -> Vect n a -> Vect (S n) a

data Elem : a -> Vect k a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

isNotInNil : Elem value [] -> Void
isNotInNil Here impossible
isNotInNil (There _) impossible

notInTail : (notHere : (value = x) -> Void) ->
            (notThere : Elem value xs -> Void) -> 
            Elem value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No isNotInNil
isElem value (x :: xs) = 
  case decEq value x of
       Yes Refl => Yes Here
       No notHere => case isElem value xs of
                          Yes prf => Yes (There prf)
                          No notThere => No (notInTail notHere notThere)

-- using Eq instead
elem : Eq ty => (value : ty) -> (xs : Vect n ty) -> Bool
elem value [] = False
elem value (x :: xs) = case value == x of
                            False => elem value xs
                            True => True
