-- Section 5.3 exercises

import Data.Vect

-- 1
readToBlank: IO (List String)
readToBlank = do
  inp <- getLine
  if inp == ""
  then pure []
  else do
    rest <- readToBlank
    pure (inp :: rest)

-- 2

concatToString : List String -> String
concatToString [] = ""
concatToString (x :: xs) = x ++ "\n" ++ concatToString xs

readAndSave : IO ()
readAndSave = do
  putStrLn "Enter file contents. When you're done enter a blank line."
  contents <- readToBlank
  putStr "Enter filename: "
  filename <- getLine
  let contents' = concatToString contents
  file' <- writeFile filename contents'
  case file' of
       (Left e) => putStrLn ("ERROR: " ++ show e)
       (Right f) => putStrLn "FILE WRITTEN"

-- 3
readFileToVect : (file : File) -> IO (n : Nat ** Vect n String)
readFileToVect file = do
  done <- fEOF file
  case done of
    True => pure (_ ** [])
    False => do
      line <- fGetLine file
      case line of
        (Left e) => do
          putStrLn ("ERROR: " ++ show e)
          pure (Z ** [])
        (Right ln) => do
          (_ ** rest) <- readFileToVect file
          pure (_ ** ln :: rest)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  file <- openFile filename Read
  case file of
    (Left e) => do
      putStrLn ("ERROR: " ++ show e)
      pure (Z ** [])
    (Right ofile) => do
      contents <- readFileToVect ofile
      closeFile ofile
      pure contents
