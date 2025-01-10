module Windows1252 exposing
  ( toUnicodeChar
  , fromUnicodeChar

  , displayClean
  , fileNameClean
  , digitaktClean
  )

import Char
import Dict
import Set


type alias Pair = (Int, Int)

mapPair : Int -> Int -> Pair
mapPair = Tuple.pair

win1252 : Pair -> Int
win1252 = Tuple.first

unicode : Pair -> Int
unicode = Tuple.second

windows1252mappings : List Pair
windows1252mappings =   -- pairs of (Windows1252, Unicode codepoint)
  [ mapPair 0x80 0x20ac
  , mapPair 0x82 0x201a
  , mapPair 0x83 0x0192
  , mapPair 0x84 0x201e
  , mapPair 0x85 0x2026
  , mapPair 0x86 0x2020
  , mapPair 0x87 0x2021
  , mapPair 0x88 0x02c6
  , mapPair 0x89 0x2030
  , mapPair 0x8a 0x0160
  , mapPair 0x8b 0x2039
  , mapPair 0x8c 0x0152
  , mapPair 0x8e 0x017d

  , mapPair 0x91 0x2018
  , mapPair 0x92 0x2019
  , mapPair 0x93 0x201c
  , mapPair 0x94 0x201d
  , mapPair 0x95 0x2022
  , mapPair 0x96 0x2013
  , mapPair 0x97 0x2014
  , mapPair 0x98 0x02dc
  , mapPair 0x99 0x2122
  , mapPair 0x9a 0x0161
  , mapPair 0x9b 0x203a
  , mapPair 0x9c 0x0153
  , mapPair 0x9e 0x017e
  , mapPair 0x9f 0x0178
  ]


windows1252replacement : Int
windows1252replacement = 0x3f     -- question mark

windows1252clamp : Int -> Int
windows1252clamp c =
  if c < 0x00 || c > 0xff then windows1252replacement else c

mapChar : (Pair -> Int) -> (Pair -> Int) -> Int -> Int
mapChar fromFn toFn c =
  case List.filter ((==) c << fromFn) windows1252mappings of
    pair :: _ -> toFn pair
    [] -> c


toUnicodeChar : Int -> Char
toUnicodeChar = windows1252clamp >> mapChar win1252 unicode >> Char.fromCode

fromUnicodeChar : Char -> Int
fromUnicodeChar = Char.toCode >> mapChar unicode win1252 >> windows1252clamp



sanitize : (Int -> Bool) -> (Int -> Int) -> String -> String
sanitize remove replace =
  String.filter (Char.toCode >> remove >> not)
  >> String.map (Char.toCode >> replace >> Char.fromCode)

isControl : Int -> Bool
isControl cp =
  cp <= 0x1f || cp == 0x7f        -- ascii control codes
  || (0x80 <= cp && cp <= 0x9f)   -- latin-1 cc1 codes

windows1252replace : Int -> Int
windows1252replace cp =
  if List.member cp displayable then cp else windows1252replacement


displayable : List Int
displayable =
  let
    ascii = List.range 0x20 0x7e
    extras = List.map unicode windows1252mappings
    latin_1 = List.range 0xa0 0xff
  in
    ascii ++ extras ++ latin_1

displayClean : String -> String
displayClean = sanitize isControl windows1252replace



fileNameReplacements : Dict.Dict Int Int
fileNameReplacements = Dict.fromList
  -- These characters aren't allowed in file systems. All but the first are
  -- Windows only restrictins.
  [ mapPair 0x2f 0x5f  -- map '/' to '_'
  , mapPair 0x22 0x27  -- map double quote to single quote
  , mapPair 0x2a 0x5f  -- map '*' to '_'
  , mapPair 0x3a 0x5f  -- map ':' to '_'
  , mapPair 0x3c 0x7b  -- map '<' to '{'
  , mapPair 0x3e 0x7d  -- map '>' to '}'
  , mapPair 0x3f 0x7e  -- map '?' to '~'
  , mapPair 0x5c 0x5f  -- map '\' to '_'
  , mapPair 0x7c 0x21  -- map '|' to '!'
  ]

fileNameReplace : Int -> Int
fileNameReplace cp = Dict.get cp fileNameReplacements |> Maybe.withDefault cp


fileNameClean : String -> String
fileNameClean = sanitize isControl (fileNameReplace << windows1252replace)


digitaktLegal : Set.Set Char
digitaktLegal =
  Set.fromList <| String.toList
  <| " !#$%&()+-"
     ++ "0123456789="
     ++ "@ABCDEFGHIJKLMNO"
     ++ "PQRSTUVWXYZ^_"
     ++ "~"
     ++ "ÄÅÆÇÑÖØÜß"

digitaktConvert : Dict.Dict Char Char
digitaktConvert =
  Dict.fromList
  <| List.map2 Tuple.pair
    (String.toList "abcdefghijklmnopqrstuvwxyzäåæçñöøü")
    (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÅÆÇÑÖØÜ")

digitaktClean : String -> String
digitaktClean =
  String.map (\c -> Dict.get c digitaktConvert |> Maybe.withDefault c)
  >> String.filter (\c -> Set.member c digitaktLegal)

