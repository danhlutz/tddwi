data InfIO : Type where
     Do : IO a
          -> (a -> Inf InfIO)
          -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

loopPrint : String -> InfIO
loopPrint msg = do
          putStrLn msg
          loopPrint msg

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

forever : Fuel
forever = More forever

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry y = putStrLn "Out of fuel"
