-- Section 10.2 exercise 3
import Data.Nat.Views

toBinary : (n : Nat) -> String
toBinary n with (halfRec n)
  toBinary Z | HalfRecZ = ""
  toBinary (x + x) | (HalfRecEven evrec) = toBinary x | evrec ++ "0"
  toBinary (S (x + x)) | (HalfRecOdd odrec) = toBinary x | odrec ++ "1"

