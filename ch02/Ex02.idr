module Main

palindrome : String -> Bool
palindrome str =
  let str' = toLower str in
  str' == reverse str'

showPalAnswer : String -> String
showPalAnswer str = show (palindrome str) ++ "\n"

main : IO ()
main = repl "Enter a string: " showPalAnswer
