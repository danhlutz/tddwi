||| Returns the number of items in a list of strings over a specified length
||| @n the specified length
||| @xs a list of strings
over_length : (n : Nat) -> (xs : List String) -> Nat
over_length n xs = length (filter is_over xs)
  where is_over str = length str > n
