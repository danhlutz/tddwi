-- section 5.1 exercises

-- 1
getLonger : String -> String -> Nat
getLonger x y = let len1 = length x
                    len2 = length y in
                case len1 > len2 of
                  False => len2
                  True  => len1

printLonger : IO ()
printLonger = do
  putStr "Input 1: "
  x <- getLine
  putStr "Input 2: "
  y <- getLine
  let longer = getLonger x y
  putStrLn ("The longer input was " ++ show longer)

-- 2
printLongerBind : IO ()
printLongerBind =
  putStr "Input 1: " >>= \_ =>
  getLine >>= \x =>
  putStr "Input 2: " >>= \_ =>
  getLine >>= \y =>
  putStrLn ("LONGER: " ++ show (getLonger x y))
