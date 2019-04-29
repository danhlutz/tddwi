data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelp : (snoc : SnocList input) -> (rest : List a) ->
               SnocList (input ++ rest)
snocListHelp {input = input} snoc [] =
             rewrite appendNilRightNeutral input in snoc
snocListHelp {input = input} snoc (x :: xs) =
             rewrite appendAssociative input [x] xs in 
             snocListHelp (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

myReverse : List a -> List a
myReverse input with (snocList input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (Snoc rec) = x :: myReverse xs | rec

