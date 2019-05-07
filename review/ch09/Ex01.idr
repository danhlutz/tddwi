-- 9.1 exercises
-- 1
data Elem : (value : a) -> List a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

test1 : Elem "Mary" ["Steve", "Fran", "Mary"]
test1 = There (There Here)

-- 2
data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value

nilNotVal : Last [] value -> Void
nilNotVal LastOne impossible
nilNotVal (LastCons _) impossible

lastNotVal : (contra : (x = value) -> Void) -> Last [x] value -> Void
lastNotVal contra LastOne = contra Refl
lastNotVal _ (LastCons LastOne) impossible
lastNotVal _ (LastCons (LastCons _)) impossible

lastNotInXs : (contra : Last (y :: xs) value -> Void) ->
              Last (x :: (y :: xs)) value -> Void
lastNotInXs contra (LastCons prf) = contra prf

total
isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No nilNotVal
isLast (x :: []) value = case decEq x value of
                              (Yes Refl) => Yes LastOne
                              (No contra) => No (lastNotVal contra)
isLast (x :: y :: xs) value =
       case isLast (y :: xs) value of
            (Yes prf) => Yes (LastCons prf)
            (No contra) => No (lastNotInXs contra)
