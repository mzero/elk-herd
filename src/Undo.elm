module Undo exposing
  ( Tag
  , tag
  , combiningTag
  , mapTag

  , Model
  , init
  , note

  , Msg
  , update
  , view
  )

{-| A simple compenent for managing an undo/redo stack of another component's
model.
-}

import Html
import Html.Attributes as Attr
import Html.Events as Events

import Html.Aria as Aria


type Tag = Tag String Bool

{-| A tag serves to name the operation a user did. The string argument is
displayed in the undo/redo drop down menus.
-}
tag : String -> Tag
tag s = Tag s False

{-| Indicates that this user operation can combine if the user does more of
them immediatly in sequence. For example, during a renaming operation, each key
stroke updates the name of item. These operations are "combining" - The user
should undo all the typing with one undo operation, not a character at a time.
-}
combiningTag : String -> Tag
combiningTag s = Tag s True

mapTag : (String -> String) -> Tag -> Tag
mapTag f (Tag s c) = Tag (f s) c

{-| The parameterized type, a, is the type of data that is checkpointed by
the undo component. Typically it is data object and some of the UI state,
like the selection.
-}
type alias Model a =
  { limit : Int
  , base : a
  , undo : List (Tag, a)
  , redo : List (Tag, a)
  }

{-| Initialize to support at most n steps of undo. The supplied value, a,
is where this history starts. If user undoes everything, this is where they
end up.
-}
init : Int -> a -> Model a
init n a = Model n a [] []

{-| Adds an operation to the undo stack, and clears the redo stack.
The value a should be the state _after_ the operation was done.
-}
note : Tag -> a -> Model a -> Model a
note t a m =
  let
    (Tag s c) = t
    rest = case m.undo of
      ((Tag s_ c_), _) :: rest_ ->
          if c && c_ &&  s == s_ then rest_ else m.undo
      _ -> m.undo
    all = (t, a) :: rest
    undo = List.take m.limit all
    base =
      case List.drop m.limit all of
        (_, a_) :: _ -> a_
        _ -> m.base
  in
    { m | base = base, undo = undo, redo = [ ] }


type Msg = Undo Int | Redo Int

update : Msg -> Model a -> (Model a, Maybe a)
update msg m =
  let
    ret m_ = case m_.undo of
      (_, a) :: _ -> (m_, Just a)
      [] -> (m_, Just m_.base)

    shuffle i from to =
      if i < 0
        then Just (from, to)
        else case from of
          ((Tag s c), a) :: rest ->
            shuffle (i - 1) rest (((Tag s False), a) :: to)
                -- undo/redo cancels its ability to combine
          [] -> Nothing
  in
    case msg of
      Undo i -> case shuffle i m.undo m.redo of
        Just (undo_, redo_) -> ret { m | undo = undo_, redo = redo_ }
        Nothing -> (m, Nothing)
      Redo i -> case shuffle i m.redo m.undo of
        Just (redo_, undo_) -> ret { m | undo = undo_, redo = redo_ }
        Nothing -> (m, Nothing)


view : Model a -> Html.Html Msg
view m =
  let
    item title msg i ((Tag s _), _) =
      Html.button
        [ Attr.type_ "button"
        , Attr.class "dropdown-item btn-sm"
        , Events.onClick (msg i)
        ]
        [ Html.text (title ++ " " ++ s) ]

    btn title msg items =
      let
        empty = List.isEmpty items
        toggleId = "toggle-btn-" ++ title
      in
        Html.div [ Attr.class "btn-group btn-group-sm" ]
          [ Html.button
            [ Attr.type_ "button"
            , Attr.class "btn btn-outline-secondary"
            , Attr.disabled empty   -- tooltip won't show if disabled
            , Events.onClick (msg 0)
            ]
            [ Html.text title ]
          , Html.button
            [ Attr.type_ "button"
            , Attr.id toggleId
            , Attr.class
                "btn btn-outline-secondary dropdown-toggle dropdown-toggle-split"
            , Attr.attribute "data-toggle" "dropdown"
            , Attr.disabled empty
            , Aria.hasPopUp True
            , Aria.expanded False
            ]
            [ Html.span
              [ Attr.class "sr-only" ]
              [ Html.text "Toggle Dropdown" ]
            ]
          , Html.div [ Attr.class "dropdown-menu", Aria.labeledBy toggleId ]
            <| List.indexedMap (item title msg) items
          ]
  in
    Html.div
      [ Attr.id "undo-panel"
      , Attr.class "btn-toolbar d-flex"
      , Aria.role "toolbar"
      ]
      [ btn "Undo" Undo m.undo
      , btn "Redo" Redo m.redo
      ]
      -- Hint: Put this view inside a Command.Subpanel item!

