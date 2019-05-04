import Data.Vect

myReverse' : Vect n elem -> Vect n elem
myReverse' [] = []
myReverse' {n = S k} (x :: xs) =
           let result = myReverse' xs ++ [x] in
           rewrite plusCommutative 1 k in result

reverseProof : Vect (len + 1) elem -> Vect (S len) elem
reverseProof {len} result =
             rewrite plusCommutative 1 len in result

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
