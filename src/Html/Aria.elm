module Html.Aria exposing
  ( role
  , controls
  , expanded
  , hasPopUp
  , hidden
  , disabled
  , label
  , labeledBy
  , values
  )

import Html exposing (Attribute)
import Html.Attributes as Attr
import Json.Encode


stringAttribute : String -> String -> Attribute msg
stringAttribute p = Attr.attribute p

boolAttribute : String -> Bool -> Attribute msg
boolAttribute p = Attr.attribute p << Json.Encode.encode 0 << Json.Encode.bool

intAttribute : String -> Int -> Attribute msg
intAttribute p = Attr.attribute p << String.fromInt


role : String -> Attribute msg
role = stringAttribute "role"


controls : String -> Attribute msg
controls = stringAttribute "aria-controls"

expanded : Bool -> Attribute msg
expanded = boolAttribute "aria-expanded"

hasPopUp : Bool -> Attribute msg
hasPopUp = boolAttribute "aria-haspopup"

hidden : Bool -> Attribute msg
hidden = boolAttribute "aria-hidden"

disabled : Bool -> Attribute msg
disabled = boolAttribute "aria-disabled"

label : String -> Attribute msg
label = stringAttribute "aria-label"

labeledBy : String -> Attribute msg
labeledBy = stringAttribute "aria-labeledby"

values : Int -> Int -> Int -> List (Attribute msg)
values min max now =
  [ intAttribute "aria-valuenmin" min
  , intAttribute "aria-valuemax" max
  , intAttribute "aria-valuenow" now
  ]
