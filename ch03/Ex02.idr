-- Matrix exercises

import Data.Vect

-- transpose
createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) ->
                  Vect n (Vect (S len) elem)
transposeHelper xs ys = zipWith (::) xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             transposeHelper x xsTrans

-- add
addRow : Num a => Vect m a -> Vect m a -> Vect m a
addRow xs ys = zipWith (+) xs ys

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a)
                  -> Vect n (Vect m a)
addMatrix xs ys = zipWith addRow xs ys

-- multiply


mulRowByRow : Num a => (x : Vect m a) -> (y : Vect m a)-> a
mulRowByRow x y = sum (zipWith (*) x y)

mulRows : Num a => (x : Vect m a)
                -> (ysTran : Vect p (Vect m a))
                -> Vect p a
mulRows x ys = map (mulRowByRow x) ys

multHelper : Num a => (xs : Vect n (Vect m a))
                   -> (ysTran : Vect p (Vect m a))
                   -> Vect n (Vect p a)
multHelper [] ysTran = []
multHelper (x :: xs) ysTran = mulRows x ysTran :: multHelper xs ysTran

multMatrix : Num a => Vect n (Vect m a)
                   -> Vect m (Vect p a)
                   -> Vect n (Vect p a)
multMatrix xs ys = let ysTran = transposeMat ys in
                       multHelper xs ysTran
