import Data.Vect

-- 1
plusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
plusCommutes Z m = rewrite sym (plusZeroRightNeutral m) in Refl
plusCommutes (S k) m = rewrite (plusCommutes k m) in
                       plusSuccRightSucc m k

-- 2
reverseProofNil : Vect k a -> Vect (plus k 0) a
reverseProofNil {k} xs = rewrite plusZeroRightNeutral k in xs

reverseProof_xs : Vect (S (n + k)) a -> Vect (n + (S k)) a
reverseProof_xs {n} {k} xs = 
  rewrite sym (plusSuccRightSucc n k) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' acc [] = reverseProofNil acc
        reverse' acc (x :: xs) = reverseProof_xs (reverse' (x::acc) xs)
