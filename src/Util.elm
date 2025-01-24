module Util exposing
  ( siString
  , hexUint, hexUint8, hexUint16, hexUint32, hexUint64
  , emptyContiguousSpaceIndex
  )

{-| A real den of odds and ends!
-}

import Array exposing (Array)
import Bitwise

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


hexDigits : Array Char
hexDigits = Array.fromList <| String.toList  "0123456789abcdef"

hexUint : Int -> Int -> String
hexUint n v =
  let
    nib i =
      Array.get (Bitwise.shiftRightZfBy (4 * i) v |> Bitwise.and 0x0f) hexDigits
      |> Maybe.withDefault ' '
  in
    List.range 0 (n // 4 - 1)
    |> List.map nib
    |> List.reverse
    |> String.fromList

hexUint8 : Int -> String
hexUint8 = hexUint 8

hexUint16 : Int -> String
hexUint16 = hexUint 16

hexUint32 : Int -> String
hexUint32 = hexUint 32

hexUint64 : Int -> Int -> String
hexUint64 hi lo = hexUint32 hi ++ "::" ++ hexUint32 lo


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



