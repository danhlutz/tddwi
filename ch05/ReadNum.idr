readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
  then pure (Just (cast input))
  else pure Nothing

readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
  do Just num1_ok <- readNumber | Nothing => pure Nothing
     Just num2_ok <- readNumber | Nothing => pure Nothing
     pure (Just (num1_ok, num2_ok))

readPair : IO (String, String)
readPair = do
  str1 <- getLine
  str2 <- getLine
  pure (str1, str2)
  
usePairVerbose : IO ()
usePairVerbose = do
  pair <- readPair
  case pair of
       (str1, str2) => putStrLn ("you entered " ++ str1 ++ " & " ++ str2)

usePair : IO ()
usePair = do
  (str1, str2) <- readPair
  putStrLn ("You entered: " ++ str1 ++ " & " ++ str2)
