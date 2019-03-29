module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
getEntry pos store
  = let store_items = items store in
        case integerToFin pos (size store) of
             Nothing => Just ("ID out of range\n", store)
             (Just id) => Just (index id store_items ++ "\n", store)

findSearchTerms : (store : DataStore) -> (term : String) -> String
findSearchTerms store term
  = let store_items = items store in
    iter store_items 0 ""
    where iter : Vect k String -> Integer -> String -> String
          iter [] i ans = ans
          iter (x :: xs) i ans
            = case (isInfixOf term x) of
                   True => (show i) ++ ": " ++ x ++ "\n"
                           ++ (iter xs (i + 1) ans)
                   False => iter xs (i + 1) ans

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
  = case parse inp of
         Nothing => Just ("Invalid Command\n", store)
         Just (Add item) =>
           Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just Size => Just ("SIZE: " ++ show (size store) ++ "\n", store)
         Just (Search term) => Just (findSearchTerms store term, store)
         Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
