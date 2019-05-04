data Vect : Nat -> Type -> Type where
     Nil : a -> Vect 0 a
     (::) : (x : a) -> Vect k a -> Vect (S k) a

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case decEq m len of
                                 (Yes Refl) => Just input
                                 (No contra) => Nothing
