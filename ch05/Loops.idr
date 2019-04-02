module Main

import System

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "LIFTOFF!"
countdown (S secs) = do
  putStrLn (show (S secs))
  usleep 100000
  countdown secs

readNumber : IO (Maybe Nat)
readNumber = do
  x <- getLine
  if all isDigit (unpack x)
  then pure (Just (cast x))
  else pure Nothing

countdowns : IO ()
countdowns = do
  putStr "Enter a starting number: "
  Just startNum <- readNumber
    | Nothing => do putStrLn "Invalid input"
  countdown startNum
  putStr "Another (y/n)? "
  yn <- getLine
  if yn == "y" then countdowns else pure ()
