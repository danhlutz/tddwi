-- section 4.2, exercise 5

import Data.Vect

addIndexes : Num a => (i : Fin n) -> (xs : Vect n a) -> (ys : Vect n a) -> a
addIndexes i xs ys = (index i xs) + (index i ys)

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys =
  let findex = integerToFin pos n in
  case findex of
       Nothing => Nothing
       (Just i) => Just (addIndexes i xs ys)
