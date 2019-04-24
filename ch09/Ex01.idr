-- Section 9.1 exercies
-- 1 Elem for list
data Elem : a -> List a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

oneInList : Elem 1 [1,2,3]
oneInList = Here

twoInList : Elem 2 [1,2,3]
twoInList = There Here

-- 2
data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

nilNotVal : Last [] value -> Void
nilNotVal LastOne impossible
nilNotVal (LastCons _) impossible

lastNotVal : (contra : (x = value) -> Void) -> Last [x] value -> Void
lastNotVal contra LastOne = contra Refl
lastNotVal _ (LastCons _) impossible

valNotConsable : (contra : Last (y :: xs) value -> Void) ->
                 Last (x :: (y :: xs)) value -> Void
valNotConsable contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No nilNotVal
isLast (x :: []) value =
       case decEq x value of
            (Yes Refl) => Yes LastOne
            (No contra) => No (lastNotVal contra)
isLast (x :: (y :: xs)) value =
       case isLast (y :: xs) value of
            (Yes prf) => Yes (LastCons prf)
            (No contra) => No (valNotConsable contra)
