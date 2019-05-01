import DataStore

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

getValues : DataStore (SString .+. val_schema) ->
            List (SchemaType val_schema)
getValues input with (storeView input)
  getValues input | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) =
            value :: getValues store | rec

