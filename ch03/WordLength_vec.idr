import Data.Vect

total allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

-- build using the search feature: SPC m p
allLen2 : Vect len String -> Vect len Nat
allLen2 [] = []
allLen2 (x :: xs) = length x :: allLen2 xs
