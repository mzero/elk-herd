module ByteArray.String exposing
  ( encodeAscii
  , decodeAscii

  , encodeWin1252
  , decodeWin1252
  )

{-| Utilities for encoding and decoding strings to `ByteArray`.

Elektron only uses Windows 1252 encoding. The Ascii encoding is given here
for completeness.
-}

import Char

import ByteArray exposing (ByteArray)
import Windows1252


encodeAscii : String -> ByteArray
encodeAscii = encode asciiFromUnicodeChar

decodeAscii : ByteArray -> String
decodeAscii = decode asciiToUnicodeChar

encodeWin1252 : String -> ByteArray
encodeWin1252 = encode Windows1252.fromUnicodeChar

decodeWin1252 : ByteArray -> String
decodeWin1252 = decode Windows1252.toUnicodeChar



-- Despite the Elm documentation, Char.toCode and fromCode deal in Unicode
-- code points up to U+FFFF.

encode : (Char -> Int) -> String -> ByteArray
encode charEncoder =
  String.toList
  >> List.map charEncoder
  >> ByteArray.fromList

decode : (Int -> Char) -> ByteArray -> String
decode charDecoder =
  ByteArray.toList
  >> List.map charDecoder
  >> String.fromList


asciiReplacement : Int
asciiReplacement = Char.toCode '?'

asciiFromUnicodeChar : Char -> Int
asciiFromUnicodeChar c =
  let
    codePoint = Char.toCode c
  in
    if 0 <= codePoint && codePoint <= 127
      then codePoint
      else asciiReplacement

asciiToUnicodeChar : Int -> Char
asciiToUnicodeChar a =
  if 0 <= a && a <= 127
    then Char.fromCode a
    else '?'