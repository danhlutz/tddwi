module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChr
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChr = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)


addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newitem
           = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) ->
                Vect (S old) (SchemaType schema)
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command : Schema -> Type where
     SetSchema : (newschema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Size : Command schema
     -- | Search String
     GetAll : Command schema
     Quit : Command schema

parsePrefix : (schema : Schema) -> String ->
              Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where getQuoted : List Char -> Maybe (String, String)
        getQuoted ('"' :: xs)
          = case span (/= '"') xs of
                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                 _ => Nothing
        getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChr input = getChar (unpack input)
  where getChar : List Char -> Maybe (Char, String)
        getChar (x :: xs) = Just (x, ltrim (pack xs))
parsePrefix (schemal .+. schemar) input = do
  (l_val, input') <- parsePrefix schemal input
  (r_val, input'') <- parsePrefix schemar input'
  Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input =
  case parsePrefix schema input of
       Just (res, "") => Just res
       Just _ => Nothing
       Nothing => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) =
  case xs of
       [] => Just SString
       _ => case parseSchema xs of
                 Nothing => Nothing
                 Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) =
  case xs of
       [] => Just SInt
       _ => case parseSchema xs of
              Nothing => Nothing
              Just xs_sch => Just (SInt .+. xs_sch)
parseSchema ("Char" :: xs) =
  case xs of
       [] => Just SChr
       _ => case parseSchema xs of
                 Nothing => Nothing
                 Just xs_sch => Just (SChr .+. xs_sch)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> (cmd : String) ->
               (args : String) -> Maybe (Command schema)
parseCommand schema "add" rest = do
  restok <- parseBySchema schema rest
  Just (Add restok)
parseCommand schema "get" "" = Just GetAll
parseCommand schema "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "size" "" = Just Size
-- parseCommand "search" str = Just (Search str)
parseCommand schema "schema" rest = do
  schemaok <- parseSchema (words rest)
  Just (SetSchema schemaok)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input =
  case span (/= ' ') input of
       (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display { schema  =  SString } item = show item
display { schema  =  SInt } item = show item
display { schema = SChr } item = show item
display { schema  =  (x .+. y) } (iteml, itemr)
  = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
getEntry pos store
  = let store_items = items store in
        case integerToFin pos (size store) of
             Nothing => Just ("ID out of range\n", store)
             (Just id) => Just (display (index id (items store)) ++ "\n",
                               store)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              (S k) => Nothing

displayAll : (schema : Schema) -> (items : Vect n (SchemaType schema)) ->
             String
displayAll schema items = iter items 0 "SHOW ALL\n"
  where iter : (items : Vect n (SchemaType schema)) ->
               Integer -> String -> String
        iter [] y sofar = sofar
        iter (x :: xs) y sofar =
          let new = show y ++ ": " ++ (display x) ++ "\n" in
          iter xs (y + 1) (sofar ++ new)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
  = case parse (schema store) inp of
         Nothing => Just ("Invalid Command\n", store)
         Just (Add item) =>
           Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just GetAll => Just (displayAll (schema store) (items store), store)
         Just (SetSchema schema') =>
              case setSchema store schema' of
                   Nothing => Just ("Can't update schema\n", store)
                   Just store' => Just ("LOADED\n", store')
         Just Size => Just ("SIZE: " ++ show (size store) ++ "\n", store)
         -- Just (Search term) => Just (findSearchTerms store term, store)
         Just Quit => Nothing

main : IO ()
main =
  replWith (MkData (SString .+. SString .+. SInt) _ []) 
           "Command: " processInput
