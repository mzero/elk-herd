module Util exposing
  ( siString
  , hexByteString
  , hexBytesString
  , emptyContiguousSpaceIndex
  )

{-| A real den of odds and ends!
-}

import Array exposing (Array)

{-| Render a file size as a short, human readable string. It picks the best
scaling factor: giga-, mega-, kilo-, or units as needed.
-}
siString : Int -> String
siString =
  let
    units unit suffix next f =
      if f / unit >= 0.1 then fmt (f / unit) ++ suffix else next f
    fmt f =
      let
        s = String.padLeft 3 '0' <| String.fromInt <| round <| 100 * f
      in
        String.dropRight 2 s ++ "." ++ String.right 2 s
  in
    (  units 1000000000 "gb"
    <| units    1000000 "mb"
    <| units       1000 "kb"
    <| units          1 "b"
    <| String.fromFloat
    ) << toFloat


hexNibble : Int -> String
hexNibble i = String.slice i (i+1) "0123456789abcdef"

hexByteString : Int -> String
hexByteString b = hexNibble (b // 16) ++ hexNibble (modBy 16 b)

hexBytesString : Int -> String
hexBytesString i =
  let
    go v r =
      if v == 0
        then r
        else go (v // 256) (hexByteString (modBy 256 v) ++ r)
  in
    if i == 0
      then "0x00"
      else "0x" ++ go i ""

emptyContiguousSpaceIndex : Bool -> Int -> Array (Maybe a) -> Maybe Int
emptyContiguousSpaceIndex banked n array =
  let
    hi = Array.length array - 1
    banks = List.map ((*) 16) <| List.range 0 (hi // 16)
    all = List.range 0 hi
    locations = (if banked then banks else []) ++ all

    available i =
      let
        k = i + n
        go j =
          if j >= k
            then True
            else case Array.get j array of
              Just Nothing -> go (j+1)
              _ -> False
      in
        go i
  in
    case List.filter available locations of
      opt :: _ -> Just opt
      [] -> Nothing



