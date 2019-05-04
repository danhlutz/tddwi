-- Section 8.2 exercises
import Data.Vect

-- 1
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in
                         (plusSuccRightSucc m k)

-- 2
reverseProofNil : Vect n a -> Vect (plus n 0) a
reverseProofNil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect ((S n) + m) a -> Vect (plus n (S m)) a
reverseProof_xs {n} {m} xs = rewrite sym (plusSuccRightSucc n m) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' acc [] = reverseProofNil acc
        reverse' acc (y :: ys) = reverseProof_xs (reverse' (y::acc) ys)
