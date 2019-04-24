-- Section 9.1 exercises, again!
-- 1
data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

elem1 : Elem 1 [2,1,3]
elem1 = There Here

-- 2
data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs val) -> Last (x :: xs) val

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

nilNotLast : Last [] value -> Void
nilNotLast LastOne impossible
nilNotLast (LastCons _) impossible

lastNotVal : (contra : (x = value) -> Void) -> Last [x] value -> Void
lastNotVal contra LastOne = contra Refl
lastNotVal _ (LastCons _) impossible

lastNotConsable : (contra : Last (y :: xs) value -> Void) ->
                  Last (x :: (y :: xs)) value -> Void
lastNotConsable contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No nilNotLast
isLast (x :: []) value = 
       case decEq x value of
            (Yes Refl) => Yes LastOne
            (No contra) => No (lastNotVal contra)
isLast (x :: (y :: xs)) value =
       case isLast (y :: xs) value of
            (Yes prf) => Yes (LastCons prf)
            (No contra) => No (lastNotConsable contra)
