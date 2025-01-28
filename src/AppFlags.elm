module AppFlags exposing (..)

{-| A lightweight flag system, without all that much rigor!

Flags are parsed from a string (which in elk-herd is stored in localStorage,
and the user can edit on the AppSetting page - but in other apps could come
from query string.)

They are key value pairs, stored as strings... but can be fetched as any
String, Bool, Int, or Float - using appropriate conversion and defualting.

Flags that are given without a value, are treated as "true"
-}

import Dict

type alias AppFlags =
  { source : String
  , flags : Dict.Dict String String
  }

init : String -> AppFlags
init input =
  let
    toEntry s =
      case String.indexes "=" s of
        [ ]       -> (String.toLower s, "true")
        (i :: _)  -> (String.toLower <| String.left i s
                     , String.dropLeft (i + 1) s
                     )

    flags =
      input
      |> String.words
      |> List.map toEntry
      |> List.filter (Tuple.first >> String.isEmpty >> not)
      |> Dict.fromList
  in
    { source = input
    , flags = flags
    }

getStringOpt : String -> AppFlags -> Maybe String
getStringOpt flag af =
  Dict.get (String.toLower flag) af.flags

getString : String -> String -> AppFlags -> String
getString flag def af =
  getStringOpt flag af |> Maybe.withDefault def

getBoolOpt : String -> AppFlags -> Maybe Bool
getBoolOpt flag af =
  Dict.get (String.toLower flag) af.flags
  |> Maybe.map (\s -> s == "true" || s == "1")

getBool : String -> Bool -> AppFlags -> Bool
getBool flag def af =
  getBoolOpt flag af |> Maybe.withDefault def

getIntOpt : String -> AppFlags -> Maybe Int
getIntOpt flag af =
  Dict.get (String.toLower flag) af.flags
  |> Maybe.andThen String.toInt

getInt : String -> Int -> AppFlags -> Int
getInt flag def af =
  getIntOpt flag af |> Maybe.withDefault def

getFloatOpt : String -> AppFlags -> Maybe Float
getFloatOpt flag af =
  Dict.get (String.toLower flag) af.flags
  |> Maybe.andThen String.toFloat

getFloat : String -> Float -> AppFlags -> Float
getFloat flag def af =
  getFloatOpt flag af |> Maybe.withDefault def
