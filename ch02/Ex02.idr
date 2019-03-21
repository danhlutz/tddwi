palindrome : String -> Bool
palindrome str =
  let str' = toLower str in
  str' == reverse str'
