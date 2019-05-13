-- Keep track of the total number of questions in the score

import Data.Primitives.Views
import System

%default total

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure val) = pure val
runCommand (Bind c f) = do
           result <- runCommand c
           runCommand (f result)

data Input = Answer Int
           | QuitCmd

readInput : (prompt : String) -> Command Input
readInput prompt = do
          PutStr prompt
          answer <- GetLine
          if toLower answer == "quit"
          then Pure QuitCmd
          else Pure (Answer (cast answer))


data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run x (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do result <- runCommand c
                              run fuel (f result)
run Dry (Do z f) = pure Nothing

partial
forever : Fuel
forever = More forever

data Score = Score' Nat Nat

Show Score where
  show (Score' k j) = show k ++ " / " ++ show j
  showPrec d x = show x

emptyScore : Score
emptyScore = Score' Z Z

mutual
  correct : Stream Int -> (score : Score) -> ConsoleIO Score
  correct nums (Score' k j)= do
    PutStr "Correct!\n"
    quiz nums (Score' (S k) (S j))

  wrong : Stream Int -> Int -> (score : Score) -> ConsoleIO Score
  wrong nums ans (Score' k j) = do
    PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
    quiz nums (Score' k (S j))

  quiz : Stream Int -> (score : Score) -> ConsoleIO Score
  quiz (num1 :: num2 :: nums) score = do
    PutStr ("Score so far: " ++ show score ++ "\n")
    input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
    case input of
         (Answer answer) => if answer == num1 * num2
                            then correct nums score
                            else wrong nums (num1 * num2) score
         QuitCmd => Quit score

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed' 

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where bound : Int -> Int
        bound x with (divides x 20)
          bound ((20 * div) + rem) | (DivBy prf) = rem + 1

partial
main : IO ()
main = do seed <- time
          Just score <- run forever
                            (quiz (arithInputs (fromInteger seed)) emptyScore)
               | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score " ++ show score)

