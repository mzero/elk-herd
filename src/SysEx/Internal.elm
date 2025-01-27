module SysEx.Internal exposing
  ( fieldView
  , hexFieldView
  , hexdumpFieldView

  , arrayTable, arrayIntTable, arrayHex16Table
  )

{-| Small utilities for building views of message fields.
-}

import Array exposing (Array)
import Html
import Html.Attributes as Attr

import ByteArray exposing (ByteArray)
import Util


fieldView : String -> String -> Html.Html msg
fieldView label value =
  Html.div [ Attr.class "field" ]
    [ Html.span [ Attr.class "label" ] [ Html.text label ]
    , Html.span [ Attr.class "value" ] [ Html.text value ]
    ]

hexFieldView : String -> String -> Html.Html msg
hexFieldView label value =
  Html.div [ Attr.class "field" ]
    [ Html.span [ Attr.class "label" ] [ Html.text label ]
    , Html.span [ Attr.class "value hexdump" ] [ Html.text value ]
    ]

hexdumpFieldView : String -> ByteArray -> Html.Html msg
hexdumpFieldView label value =
  Html.div [ Attr.class "field field-fullwidth" ]
    [ Html.span [ Attr.class "label" ] [ Html.text label ]
    , Html.span [ Attr.class "value hexdump" ]
      [ Html.text <| ByteArray.hexDump value ]
    ]


arrayTable : (a -> Html.Html msg) -> Int -> Array a -> Html.Html msg
arrayTable cellFn span items =
  let
    indexCell i =
      Html.td [ Attr.class "index" ]
        [ Html.text <| "[ " ++ String.fromInt i ++ " ]"]
    dataCell i =
      Html.td [ ]
        ( Array.get i items
        |> Maybe.map (cellFn >> List.singleton)
        |> Maybe.withDefault  [ ]
        )
    doRow i =
      if i >= Array.length items
        then [ ]
        else
          Html.tr [ ]
            ( indexCell i
            :: List.map dataCell (List.range i (i + span - 1))
            )
          :: doRow (i + span)
  in
    Html.table [ Attr.class "array" ] <| doRow 0

arrayIntTable : Int -> Array Int -> Html.Html msg
arrayIntTable = arrayTable (Html.text << String.fromInt)

arrayHex16Table : Int -> Array Int -> Html.Html msg
arrayHex16Table = arrayTable (Html.text << Util.hexUint16)