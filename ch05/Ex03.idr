-- section 5.2, exercise 4

myRepl : (prompt : String) -> (op : String -> String) -> IO ()
myRepl prompt op= do
  putStr prompt
  input <- getLine
  putStrLn (op input)
  myRepl prompt op

myReplWith : (state : a) -> (prompt : String) ->
             (op : a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt op = do
  putStr prompt
  input <- getLine
  let result = op state input
  case result of
       Nothing => putStrLn "EXITING"
       Just (output, newState) => do
         putStrLn output
         myReplWith newState prompt op

-- testing functions
strLen : String -> String
strLen str = show (length str)

sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp =
  if all isDigit (unpack inp)
  then let newTot = tot + (cast inp) in Just (show newTot, newTot)
  else Nothing
