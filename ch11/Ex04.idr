-- Section 11.3 exercises 2 and 3

import Data.Primitives.Views
import System

%default total 

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     ReadFile : String -> Command (Either FileError String)
     WriteFile : String -> String -> Command (Either FileError ())

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile filename) = readFile filename
runCommand (WriteFile filename input) = writeFile filename input

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run x (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do result <- runCommand c
                              run fuel (f result)
run Dry (Do z f) = pure Nothing

partial
forever : Fuel
forever = More forever

mutual
  myCat : (prompt : String) -> (filename : String) -> ConsoleIO ()
  myCat prompt filename = do
    contents <- ReadFile filename
    case contents of
         (Left l) => do
           PutStr (show l ++ "\n")
           myShell prompt
         (Right msg) => do
           PutStr msg
           myShell prompt

  myCopy : (prompt : String) -> (source : String) -> (destination : String) ->
           ConsoleIO ()
  myCopy prompt source destination = do
         toCopy <- ReadFile source
         case toCopy of
              (Left err) => do
                    PutStr (show err ++ "\n")
                    myShell prompt
              (Right toWrite) => do
                     result <- WriteFile destination toWrite
                     case result of
                          (Left wrterr) => do
                                PutStr (show wrterr ++ "\n")
                                myShell prompt
                          (Right success) => do
                                 PutStr ("FILE WRITTEN!\n")
                                 myShell prompt

  myError : (prompt : String) -> ConsoleIO ()
  myError prompt = do
      PutStr "Unrecognized command. Commands are: \n"
      PutStr "cat [filename]\n"
      PutStr "copy [source] [destination]\n"
      PutStr "quit"
      myShell prompt


  myShell : (prompt : String) -> ConsoleIO ()
  myShell prompt = do
    PutStr prompt
    response <- GetLine
    let response' = words response
    case response' of
        ["quit"] => Quit ()
        ["cat", filename] => myCat prompt filename
        ["copy", source, destination] => myCopy prompt source destination
        _ => myError prompt

partial
main : IO ()
main = do 
    Just discard <- run forever (myShell "LAMBDA: ")
        | Nothing => putStrLn "Ran out of fuel"
    putStrLn "bye bye"

