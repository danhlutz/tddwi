||| Return the top ten biggest items in a list
||| @xs List items to be sorted and returned
top_ten : Ord a => (xs : List a) -> List a
top_ten xs =
  let backSorted = reverse (sort xs)
  in take 10 xs
