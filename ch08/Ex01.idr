-- rewrite checkEqNat using case splits

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
     Same : (num : Nat) -> EqNat num num

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = 
  case checkEqNat k j of
       Nothing => Nothing
       Just (Same j) => Just (Same (S j))
