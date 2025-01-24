module Elektron.Struct.Part exposing
  ( Decoder, Encoder

  , Part

  , uint8, uint16be, uint32be
  , bytes
  , chars

  , list, array, arrayHex
  , map

  , const, ephemeral, optional

  , magicHead, magicTail
  , fail
  )

{-| This is somewhat like Codec, but a bit more.

This code is very similar to the code in `SysEx.ApiUtil`. In fact, this code's
`Part` and that code's `Arg` are almost identical. It could probably all be
unified and put under `ByteArray`... but not now.
-}

import Array exposing (Array)
import Html
import Html.Attributes as Attr

import ByteArray exposing (ByteArray)
import ByteArray.Builder as Builder
import ByteArray.Parser as Parser
import Missing.List as List
import SysEx.Internal exposing (..)
import Util
import Windows1252

type alias Decoder a = Parser.Parser a
type alias Encoder = Builder.Builder


{-| An object that encapsulates how to encode, decode, and view a particular
data type. All three fields are functions.

The `view` function here takes a string which is the name of the field that
this datum is for.

TODO: `Arg.view` doesn't have this extra string label. `Part` probably shouldn't
either, and the "fieldness" of a value should handled over in `field`. Or at
least in a transformer of `Part -> LabeledPart`?
-}
type alias Part a =
  { encoder : a -> Encoder
  , decoder : Decoder a
  , view : String -> a -> List (Html.Html Never)
  }

uint8 : Part Int
uint8 =
  { encoder = Builder.uint8
  , decoder = Parser.uint8
  , view = \label v -> [ fieldView label <| String.fromInt v ]
  }


uint16be : Part Int
uint16be =
  { encoder = Builder.uint16be
  , decoder = Parser.uint16be
  , view = \label v -> [ fieldView label <| String.fromInt v ]
  }

uint32be : Part Int
uint32be =
  { encoder = Builder.uint32be
  , decoder = Parser.uint32be
  , view = \label v -> [ fieldView label <| String.fromInt v ]
  }

bytes : Int -> Part ByteArray
bytes n =
  { encoder = Builder.bytes
  , decoder = Parser.bytes n
  , view = \label v ->
    [ Html.div [ Attr.class "field field-fullwidth" ]
      [ Html.span [ Attr.class "label" ] [ Html.text label ]
      , Html.span [ Attr.class "value hexdump" ]
        [ Html.text <| ByteArray.hexDump v ]
      ]
    ]
  }

chars : Int -> Part String
chars length =
  let
    extractString :  ByteArray -> String
    extractString =
      ByteArray.toList
      >> List.filter ((/=) 0)
      >> List.map Windows1252.toUnicodeChar
      >> String.fromList
    setString : String -> ByteArray
    setString =
      Windows1252.digitaktClean
      >> String.padRight length '\u{0000}'
      >> String.left length
      >> String.toList
      >> List.map Windows1252.fromUnicodeChar
      >> ByteArray.fromList
  in
    { encoder = setString >> Builder.bytes
    , decoder = Parser.bytes length |> Parser.map (extractString)
    , view = \label v -> [ fieldView label <| "\"" ++ v ++ "\"" ]
    }



map : (a -> b) -> (b -> a) -> Part a -> Part b
map fab fba pa =
  { encoder = pa.encoder << fba
  , decoder = Parser.map fab pa.decoder
  , view = \label v -> pa.view label <| fba v
  }

{-| A part that is expecting to always be the same value. Believe it or not,
there are a few of these in the Instrument structures!
-}
const : a -> String -> Part a -> Part a
const c desc pa =
  { encoder = always (pa.encoder c)
  , decoder =
      pa.decoder |> Parser.andThen (\v ->
        if v == c
          then Parser.succeed v
          else Parser.fail <| "expecting " ++ desc
      )
  , view = pa.view
  }

{-| A part that isn't serialized or viewed.
-}
ephemeral : a -> Part a
ephemeral a =
  { encoder = \_ -> Builder.empty
  , decoder = Parser.succeed a
  , view = \label v -> [ ]
  }


optional : Part a -> Part (Maybe a)
optional pa =
  { encoder = Maybe.map pa.encoder >> Maybe.withDefault Builder.empty
  , decoder = Parser.optional pa.decoder
  , view = \label -> Maybe.map (pa.view label) >> Maybe.withDefault [ ]
  }


list : Int -> Part a -> Part (List a)
list n pa =
  { encoder = Builder.list pa.encoder
  , decoder =
    let
      step (i, r) =
        if i <= 0
          then Parser.succeed <| List.reverse r
          else pa.decoder |> Parser.andThen (\e -> step (i - 1, e::r))
    in
      step (n, [])
  , view = \label v ->
    [ Html.div [ Attr.class "field field-fullwidth" ]
      [ Html.span [ Attr.class "label" ] [ Html.text label ]
      , Html.ol [ Attr.class "field field-fullwidth" ]
        <| List.map (Html.li [] << pa.view "") v
      ]
    ]
  }

array : Int -> Part a -> Part (Array a)
array n pa =
  let
    pl = list n pa
  in
    { encoder = Builder.array pa.encoder
    , decoder = Parser.map Array.fromList pl.decoder
    , view = \label v -> pl.view label <| Array.toList v
   }


arrayHex : Int -> Part Int -> Part (Array Int)
arrayHex n pa =
  let
    pl = list n pa
  in
    { encoder = Builder.array pa.encoder
    , decoder = Parser.map Array.fromList pl.decoder
    , view = \label v ->
      [ Html.div [ Attr.class "field field-fullwidth" ]
        [ Html.span [ Attr.class "label" ] [ Html.text label ]
        , arrayHexTable 16 v
        ]
      ]
    }


{-| A field that always has a constant, magic value.
-}
magic : Int -> Part Int
magic x = const x ("magic " ++ Util.hexUint32 x) uint32be

magicHead : Part Int
magicHead = magic 0xbeefbace

magicTail : Part Int
magicTail = magic 0xbacef00c


fail : String -> Part a
fail msg =
  { encoder = always Builder.empty
  , decoder = Parser.fail msg
  , view = \label _ -> [ fieldView label ("failed: " ++ msg) ]
  }
