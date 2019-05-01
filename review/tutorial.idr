-- online tutorial work

-- Equality
fiveIsFive : 5 = 5
fiveIsFive = Refl

twoPlusTwo : 2 + 2 = 4
twoPlusTwo = Refl

-- Void
disjoint : (n : Nat) -> Z = S n -> Void
disjoint n prf = replace {P = disjointTy} prf ()
  where
    disjointTy : Nat -> Type
    disjointTy Z = ()
    disjointTy (S k) = Void

-- Theorems
plusReduces : (n : Nat) -> plus Z n = n
plusReduces n = Refl

plusReducesZ : (n : Nat) -> n = plus n Z
plusReducesZ Z = Refl
plusReducesZ (S k) = cong (plusReducesZ k)

-- Theorem proving
data Parity : Nat -> Type where
     Even : Parity (n + n)
     Odd  : Parity (S (n + n))

helpEven : (k : Nat) -> (result : Parity (S k + S k)) ->
           Parity (S (S (plus k k)))
helpEven k result = rewrite plusSuccRightSucc k k in result

helpOdd : (k : Nat) -> Parity (S ((S k) + (S k))) ->
          Parity (S (S (S (plus k k))))
helpOdd k x = rewrite (plusSuccRightSucc k k) in x

parity : (n : Nat) -> Parity n
parity Z = Even {n = 0}
parity (S Z) = Odd {n = 0}
parity (S (S k)) with (parity k)
  parity (S (S (k + k))) | Even = let result = Even {n = S k} in
                                      helpEven k result
  parity (S (S (S (k + k)))) | Odd = let result = Odd {n = S k} in
                                         helpOdd k result


natToBin : Nat -> List Bool
natToBin k with (parity k)
  natToBin (n + n) | Even = False :: natToBin n
  natToBin (S (n + n)) | Odd = True :: natToBin n

-- hints for totality
qsort : Ord a => List a -> List a
qsort [] = []
qsort (x :: xs) = qsort (filter (< x) xs) ++
      (x :: qsort (filter (>= x) xs))

total
qsort' : Ord a => List a -> List a
qsort' [] = []
qsort' (x :: xs) =
       qsort' (assert_smaller (x :: xs) (filter (< x) xs)) ++
         (x :: qsort' (assert_smaller (x :: xs) (filter (>= x) xs)))
