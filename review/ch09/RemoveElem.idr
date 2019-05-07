import Data.Vect

-- removeElem : DecEq a => (value : a) -> (xs : Vect (S n) a) -> Vect n a
-- removeElem value (x :: xs) = case decEq value x of
--                                  (Yes prf) => xs
--                                  (No contra) => x :: removeElem value xs

removeElem : (value : a) ->
             (xs : Vect (S n) a) ->
             (prf : Elem value xs) ->
             Vect n a
removeElem value (value :: ys) Here = ys
removeElem {n = Z} value (y :: []) (There later) = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) =
           y :: removeElem value ys later

removeElem_auto : (value : a) -> (xs : Vect (S n) a) ->
                  {auto prf : Elem value xs} -> Vect n a
removeElem_auto value (value :: ys) {prf = Here} = ys
removeElem_auto {n = Z} value (y :: []) {prf = (There later)} = absurd later
removeElem_auto {n = (S k)} value (y :: ys) {prf = (There later)} =
                y :: removeElem_auto value ys
