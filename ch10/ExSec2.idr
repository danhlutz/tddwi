-- Section 10.2 exercises, second go around

import Data.Vect
import Data.List.Views
import Data.Vect.Views
import Data.Nat.Views

-- 1
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
mergeSort : Ord a => Vect n a -> Vect n a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (xs ++ ys) | (SplitRecPair lrec rrec) =
            merge (mergeSort xs | lrec)
                  (mergeSort ys | rrec)

-- 3
toBinary : (n : Nat) -> String
toBinary n with (halfRec n)
  toBinary Z | HalfRecZ = ""
  toBinary (x + x) | (HalfRecEven evenrec) = toBinary x | evenrec ++ "0"
  toBinary (S (x + x)) | (HalfRecOdd oddrec) = toBinary x | oddrec ++ "1"

-- 4
palindrome : Eq a => (input : List a) -> Bool
palindrome input with (vList input)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | (VCons rec) =
             if x == y
             then palindrome xs | rec
             else False


