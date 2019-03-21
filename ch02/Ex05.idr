||| Returns the number of words in a string, and its total length
counts : String -> (Nat, Nat)
counts str =
  let numWords = length (words str) in
  (numWords, length str)
