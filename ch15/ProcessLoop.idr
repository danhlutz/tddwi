import System.Concurrency.Channels

data Message = Add Nat Nat

data MessagePID = MkMessage PID

data Process : Type -> Type where
     Action : IO a -> Process a
     Spawn : Process () -> Process (Maybe MessagePID)
     Loop : Inf (Process a) -> Process a
     Request : MessagePID -> Message -> Process (Maybe Nat)
     Respond : ((msg : Message) -> Process Nat) -> Process (Maybe Message)
     Pure : a -> Process a
     (>>=) : Process a -> (a -> Process b) -> Process b

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> Process t -> IO (Maybe t)
run Dry _ = pure Nothing
run fuel (Request (MkMessage process) msg) = do
  Just chan <- connect process
       | _ => pure (Just Nothing)
  ok <- unsafeSend chan msg
  if ok
  then do Just x <- unsafeRecv Nat chan
               | Nothing => pure (Just Nothing)
          pure (Just (Just x))
  else pure (Just Nothing)
run fuel (Respond calc) = do
    Just sender <- listen 1
         | Nothing => pure (Just Nothing)
    Just msg <- unsafeRecv Message sender
         | Nothing => pure (Just Nothing)
    Just res <- run fuel (calc msg)
         | Nothing => pure Nothing
    unsafeSend sender res
    pure (Just (Just msg))
run (More fuel) (Loop act) = run fuel act
run fuel (Spawn proc) = do
  Just pid <- spawn (do run fuel proc
                        pure ())
       | Nothing => pure Nothing
  pure (Just (Just (MkMessage pid)))
run fuel (Action act) = do res <- act
                           pure (Just res)
run fuel (Pure x) = pure (Just x)
run fuel (act >>= next) = do
    Just res <- run fuel act
         | Nothing => pure Nothing
    run fuel (next res)

partial
forever : Fuel
forever = More forever

partial
runProc : Process () -> IO ()
runProc proc = do run forever proc
                  pure ()

procAdder : Process ()
procAdder = do Respond (\msg => case msg of
                                (Add k j) => Pure (k + j))
               Loop procAdder

procMain : Process ()
procMain = do Just adder_id <- Spawn procAdder
                   | Nothing => Action (putStrLn "Spawn failed")
              Just answer <- Request adder_id (Add 2 3)
                   | Nothing => Action (putStrLn "Request failed")
              Action (printLn answer)
