-- Chapter 14.2, exercise 1

data Access = LoggedOut | LoggedIn
data PwdCheck = Correct | Incorrect

data ShellCmd : (ty : Type) -> Access -> (ty -> Access) -> Type where
     Password : String ->
                ShellCmd PwdCheck LoggedOut
                                  (\pw => case pw of
                                          Correct => LoggedIn
                                          Incorrect => LoggedOut)
     Logout : ShellCmd () LoggedIn (const LoggedOut)
     GetSecret : ShellCmd String LoggedIn (const LoggedIn)
 
     PutStr : String -> ShellCmd () state (const state)
     Pure : (res : ty) -> ShellCmd ty (state_fn res) state_fn
     (>>=) : ShellCmd a state1 state_fn2 ->
             ((res : a) -> ShellCmd b (state_fn2 res) state_fn3) ->
             ShellCmd b state1 state_fn3

session : ShellCmd () LoggedOut (const LoggedOut)
session = do Correct <- Password "wurzel"
                   | Incorrect => PutStr "Wrong password"
             msg <- GetSecret
             PutStr ("Secret code: " ++ show msg)
             Logout
