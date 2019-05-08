-- section 10.2 exercises
import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views

-- 1
total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix input1 input2 with (snocList input1)
  equalSuffix [] input2 | Empty = []
  equalSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    equalSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) = 
                if x == y
                then equalSuffix xs ys | xsrec | ysrec ++ [x]
                else []

-- 2
total
mergeSort : Ord a => (input : Vect n a) -> Vect n a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (xs ++ ys) | (SplitRecPair lrec rrec) =
            merge (mergeSort xs | lrec) (mergeSort ys | rrec)

-- 3
toBinary : (input : Nat) -> String
toBinary input with (halfRec input)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = toBinary n | rec ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = toBinary n | rec ++ "1"

-- 4
palindrome : DecEq a => (input : List a) -> Bool
palindrome input with (vList input)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | (VCons rec) =
             case decEq x y of
                  (Yes prf) => palindrome xs | rec
                  (No contra) => False

