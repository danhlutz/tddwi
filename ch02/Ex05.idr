module Main

||| Returns the number of words in a string, and its total length
counts : String -> (Nat, Nat)
counts str =
  let numWords = length (words str) in
  (numWords, length str)

countToString : String -> String
countToString str = show (counts str) ++ "\n"

main : IO ()
main = repl "Words to count: " countToString
