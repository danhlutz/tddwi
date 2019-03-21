||| Check if a string is a palindrome of a defined length or greater
||| @x The minimum length of the palindrome
||| @str The string to be checked
palindrome : (x : Nat) -> (str : String) -> Bool
palindrome x str =
  let str' = toLower str in
      str' == reverse str' 
  && (length str) > x
