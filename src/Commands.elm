module Commands exposing
  ( Item(..)
  , Group
  , Commands
  , map

  , view
  )

{-| A utility to up a command panel from parts.

The command panel is the area on the left, containing commands for the user
to click on.  The panel has a list of groups, and each group is a list of
items. Items can be:

  * a link (anchor element) to open another page
  * a button, sending some message when clicked
  * a load file button, which opens a file dialog, and sends a message with
    the user chosen files
  * a subpanel with arbitrary HTML in it

It is common for various modules to export `commands`, which is their
`Group` of items.  On the page's view these can be combined into a list
and passed to `view` to generate the Html.

When combining, use `map` the message types in the normal manner.
-}

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as D

import Html.Aria as Aria


type Item msg
  = Anchor   String String Bool String   -- id, label, enabled, url
  | Button   String String Bool msg      -- id, label, enabled, msg
  | LoadFile String String Bool (List (String, D.Value) -> msg) String Bool
      -- id, label, enabled, msgFn, accept, multi
  | Subpanel (Html.Html msg)

type alias Group msg = { label : String, items : List (Item msg) }


filesDecoder : D.Decoder (List (String, D.Value))
filesDecoder =
  let
    item = D.map2 Tuple.pair (D.field "name" D.string) D.value
  in
    D.at ["target", "files"] (D.list item)

mapItem : (m1 -> m2) -> Item m1 -> Item m2
mapItem f item = case item of
  Anchor   i l e u      -> Anchor   i l e u
  Button   i l e m      -> Button   i l e (f m)
  LoadFile i l e mf a n -> LoadFile i l e (f << mf) a n
  Subpanel h            -> Subpanel (Html.map f h)


type alias Commands msg = List (Group msg)


map : (m1 -> m2) -> Commands m1 -> Commands m2
map f = List.map (\g -> Group g.label (List.map (mapItem f) g.items))


view : Commands msg -> Html.Html msg
view cmds =
  let
    hiddenInputId s = "hidden-input-" ++ s

    btnText text =
      [ Html.span [ Attr.class "cmd-icon" ] [ ]
      , Html.span [ Attr.class "cmd-text d-none d-lg-inline" ] [ Html.text text ]
      ]

    item i = case i of
      Anchor id text enabled url ->
        Html.a
          [ Attr.class "btn btn-outline-secondary btn-block cmd-button"
          , Attr.classList [ ("disabled", not enabled) ]
          , Attr.id id
          , Attr.href url
          , Attr.target "_blank"
          , Attr.rel "noopener noreferer"
          , Aria.disabled (not enabled)
          , Aria.role "button"
          ]
          (btnText text)

      Button id text enabled command ->
        Html.button
          [ Attr.class "btn btn-outline-secondary btn-block cmd-button"
          , Attr.id id
          , Attr.type_ "button"
          , Attr.disabled (not enabled)
          , Events.onClick command
          ]
          (btnText text)

      LoadFile id text enabled commandFn accept _ ->
        Html.label
          [ Attr.class "btn btn-outline-secondary btn-block cmd-button"
          , Attr.classList [ ("disabled", not enabled) ]
          , Attr.id id
          , Attr.tabindex 0
          , Attr.for (hiddenInputId id)
          , Aria.disabled (not enabled)
          , Aria.role "button"
          ]
          (btnText text)
      Subpanel h ->
          h

    group g =
      Html.div
        [ Attr.class "btn-group-vertical"
        , Aria.role "group"
        , Aria.label g.label
        ]
        <| List.map item g.items

    extra i = case i of
      Anchor _ _ _ _ -> Nothing
      Button _ _ _ _ -> Nothing
      LoadFile id text enabled commandFn accept multi -> Just <|
        Html.input
          [ Attr.id (hiddenInputId id)
          , Attr.disabled (not enabled)
          , Events.on "change" (D.map commandFn filesDecoder)
          , Attr.type_ "file"
          , Attr.value ""
          , Attr.accept accept
          , Attr.multiple multi
          , Attr.style "visibility" "hidden"
          , Attr.style "height" "1px"
          , Aria.hidden True
          ]
          [ ]
      Subpanel _ -> Nothing

    extras g = List.filterMap extra g.items

    cmds_ = List.filter (not << List.isEmpty << .items) cmds
  in
    Html.div [ Attr.class "cmd-section" ]
      <| List.map group cmds_ ++ List.concatMap extras cmds_


