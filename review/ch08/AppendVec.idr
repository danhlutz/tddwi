import Data.Vect

appendNil : (ys : Vect m elem)-> Vect (plus m 0) elem
appendNil {m} ys = rewrite plusZeroRightNeutral m in ys

append_xs : Vect (S (m + len)) elem -> Vect (plus m (S len)) elem
append_xs {m} {len} xs = rewrite sym (plusSuccRightSucc m len) in xs

append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = appendNil ys
append (x :: xs) ys = append_xs (x :: append xs ys)
