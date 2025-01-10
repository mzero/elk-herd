module SysEx.Internal exposing
  ( fieldView
  , hexFieldView
  , hexdumpFieldView
  )

{-| Small utilities for building views of message fields.
-}

import Html
import Html.Attributes as Attr

import ByteArray exposing (ByteArray)



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
