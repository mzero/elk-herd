module Missing.Html.Events exposing
  ( onExclusive

  , Position
  , position

  , MouseEvent
  , mouseEvent
  
  , onMouseDownExtended
  , onMouseUpExtended
  , onMouseMoveExtended

  , onDragEnter
  , onDragOverIgnore
  , onDragLeave

  , onKeyDown
  )

import Html
import Html.Events
import Json.Decode as D


onExclusive : String -> D.Decoder msg -> Html.Attribute msg
onExclusive evt d = Html.Events.custom evt <| D.map
    (\m -> { message = m, stopPropagation = True, preventDefault = True }) d


{- Extended Mouse Events -}


type alias Position = { x : Int, y : Int }

position : D.Decoder Position
position = D.map2 Position (D.field "clientX" D.int) (D.field "clientY" D.int)


type alias MouseEvent =
  { type_ : String
  , xy : Position
  , shift : Bool
  , meta : Bool
  }

mouseEvent : D.Decoder MouseEvent
mouseEvent =
  D.map4 MouseEvent
    (D.field "type" D.string)
    position
    (D.field "shiftKey" D.bool)
    (D.field "metaKey" D.bool)

onMouseEventExtended : String -> (MouseEvent -> msg) -> Html.Attribute msg
onMouseEventExtended eventName fMsg =
  onExclusive eventName (D.map fMsg mouseEvent)

onMouseDownExtended : (MouseEvent -> msg) -> Html.Attribute msg
onMouseDownExtended = onMouseEventExtended "mousedown"

onMouseUpExtended : (MouseEvent -> msg) -> Html.Attribute msg
onMouseUpExtended = onMouseEventExtended "mouseup"

onMouseMoveExtended : (MouseEvent -> msg) -> Html.Attribute msg
onMouseMoveExtended = onMouseEventExtended "mousemove"

{- Drag Events -}

onDragEnter : msg -> Html.Attribute msg
onDragEnter msg = onExclusive "dragenter" (D.succeed msg)

onDragOverIgnore : Html.Attribute msg
onDragOverIgnore = onExclusive "dragover" (D.fail "don't care")

onDragLeave : msg -> Html.Attribute msg
onDragLeave msg = onExclusive "dragleave" (D.succeed msg)

{- Key events -}

onKeyDown : (String -> msg) -> Html.Attribute msg
onKeyDown msg = Html.Events.on "keydown" (D.map msg (D.field "key" D.string))

