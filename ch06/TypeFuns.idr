StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "ninety-four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False x = trim x
valToString True x = cast x
