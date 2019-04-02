-- section 5.2 exercises

module Main

import System

getGuess : IO (Maybe Nat)
getGuess = do
  x <- getLine
  if all isDigit (unpack x)
  then pure (Just (cast x))
  else pure Nothing

guess : (target : Nat) -> (guesses: Nat) -> IO ()
guess target guesses = do
  putStr ("G# " ++ show guesses ++ " | Guess a number! ")
  (Just input) <- getGuess | Nothing => do putStrLn "INVALID ANSWER"
                                           guess target (S guesses)
  case compare input target of
       LT => do
         putStrLn "Your guess is too low!"
         guess target (S guesses)
       EQ => do
         putStrLn "You won!"
       GT => do
         putStrLn "Your guess is too high!"
         guess target (S guesses)

main : IO ()
main = do
  cTime <- time
  let target = (mod cTime 100) + 1
  guess (cast target) Z
