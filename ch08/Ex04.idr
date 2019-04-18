import Data.Vect

-- 1
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in
                         (plusSuccRightSucc m k)

-- 2
reverseProof_nil : Vect k a -> Vect (plus k 0) a
reverseProof_nil {k} xs =
  rewrite plusZeroRightNeutral k in xs


reverseProof_xs : Vect ((S j) + k) a -> Vect (plus j (S k)) a
reverseProof_xs {j} {k} xs =
  rewrite sym (plusSuccRightSucc j k) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs) = reverseProof_xs (reverse' (x::acc) xs)
