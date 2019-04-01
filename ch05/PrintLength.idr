
printLength : IO ()
printLength = putStr "Input: " >>= \_ =>
              getLine >>= \input =>
              let len = length input in
              putStrLn (show len)

printLength2 : IO ()
printLength2 = do
  putStr "Input: "
  x <- getLine
  let len = length x
  putStrLn (show len)
