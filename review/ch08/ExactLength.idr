data Vect : Nat -> Type -> Type where
     Nil  : a -> Vect Z a
     (::) : (x : a) -> Vect n a -> Vect (S n) a

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
     Same : (num : Nat) -> EqNat num num

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = do
           Same j <- checkEqNat k j
           pure (Same (S j))

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = do
            Same len <- checkEqNat m len
            pure input
