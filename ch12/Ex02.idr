-- Section 12.3, exercises 1 and 2

import Data.Primitives.Views
import System

%default total

record Score where
       constructor MkScore
       correct : Nat
       attempted : Nat

record GameState where
       constructor MkGameState
       score : Score
       difficulty : Int

Show GameState where
  show st = show (correct (score st)) ++ "/" ++
            show (attempted (score st)) ++ "\n" ++
            "Difficulty: " ++ show (difficulty st)

initState : GameState
initState = MkGameState (MkScore 0 0) 12

setDifficulty : Int -> GameState -> GameState
setDifficulty new = record { difficulty = new }

addWrong : GameState -> GameState
addWrong = record { score->attempted $= (+1) }

addCorrect : GameState -> GameState
addCorrect = record { score->correct $= (+1),
                      score->attempted $= (+1)}

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String

     GetRandom : Command Int
     GetGameState : Command GameState
     PutGameState : GameState -> Command ()

     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

-- exercise -> define Functor, Applicative and Monad for Command
Functor Command where
  map func (PutStr x) = Bind (PutStr x) (\res => Pure (func res))
  map func GetLine = Bind GetLine (\line => Pure (func line))
  map func GetRandom = Bind GetRandom (\r => Pure (func r))
  map func GetGameState = Bind GetGameState (\st => Pure (func st))
  map func (PutGameState x) = Bind (PutGameState x) (\st => Pure (func st))
  map func (Pure x) = Pure (func x)
  map func (Bind ca' fapc) =
    Bind ca' (\a' =>
      Bind (fapc a') (\a'' => Pure (func a'')))

Applicative Command where
  pure = Pure
  (<*>) cab ca =
    Bind cab (\fab =>
      Bind ca (\a' => pure (fab a')))

Monad Command where
  (>>=) = Bind

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDO
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

runCommand : Stream Int -> GameState -> Command a ->
             IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do
  putStr x
  pure ((), rnds, state)
runCommand rnds state GetLine = do
  str <- getLine
  pure (str, rnds, state)
runCommand (val :: rnds) state GetRandom =
  pure (getRandom val (difficulty state), rnds, state)
  where
    getRandom : Int -> Int -> Int
    getRandom val max with (divides val max)
      getRandom val 0 | DivByZero = 1
      getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1
runCommand rnds state GetGameState = pure (state, rnds, state)
runCommand rnds state (PutGameState newState) = pure ((), rnds, newState)
runCommand rnds state (Pure x) = pure (x, rnds, state)
runCommand rnds state (Bind c f) = do
  (res, newRnds, newState) <- runCommand rnds state c
  runCommand newRnds newState (f res)

run : Fuel -> Stream Int -> GameState -> ConsoleIO a ->
      IO (Maybe a, Stream Int, GameState)
run fuel rnds state (Quit val) = do  pure (Just val, rnds, state)
run (More fuel) rnds state (Do c f) = do
  (res, newRnds, newState) <- runCommand rnds state c
  run fuel newRnds newState (f res)
run Dry rnds state p = pure (Nothing, rnds, state)

updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do
  st <- GetGameState
  PutGameState (f st) 


mutual
  correct : ConsoleIO GameState
  correct = do
    PutStr "Correct!\n"
    updateGameState addCorrect
    quiz

  wrong : Int -> ConsoleIO GameState
  wrong ans = do
    PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
    updateGameState addWrong
    quiz

  data Input = Answer Int | QuitCmd

  readInput : (prompt : String) -> Command Input
  readInput prompt = do
    PutStr prompt
    answer <- GetLine
    if toLower answer == "quit"
    then Pure QuitCmd
    else Pure (Answer (cast answer))

  quiz : ConsoleIO GameState
  quiz = do
    num1 <- GetRandom
    num2 <- GetRandom
    st <- GetGameState
    PutStr (show st ++ "\n")
    input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
    case input of
         (Answer ans) => if ans == num1 * num2
                         then correct
                         else wrong (num1 * num2)
         QuitCmd => Quit st

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in 
               (seed' `shiftR` 2) :: randoms seed'

partial
main : IO ()
main = do
  seed <- time
  (Just score, _, state) <-
        run forever (randoms (fromInteger seed)) initState quiz
            | _ => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show state)
