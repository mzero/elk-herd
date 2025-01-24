module Project.Selection.Bank exposing
  ( Selection
  , initSelectOnly
  , initDraggable
  , initExtendable

  , select
  , selected
  , firstSelected
  , isSelected
  , anySelected

  , isDragging
  , DragInfo
  , dragInfo

  , DropInfo
  , dropInfo

  , Msg
  , isMouseDown
  , update
  , itemHandlers
  , subscriptions
  )

{-| Handling selection within a bank. Note that everything in this module
is parameterized by the type of thing that is in the bank. This ensures we
don't confuse one kind of selection with another.

See `Project.Selection.Project` for how these are all integrated.
-}

import Browser.Events
import Html
import Html.Events as Events
import Json.Decode as D

import Bank exposing (Index(..))
import Bank.IndexSet as IndexSet exposing (IndexSet)
import Missing.Html.Events as Events exposing (MouseEvent, Position)
import Missing.Maybe as Maybe

type Style = SelectOnly | SelectWithDrag | SelectExtend

type Mode i
  = Hover
  | Select
  | Drag Position (Maybe (Index i))
  | Drop (Index i)

type Action i
  = NoAction
  | Deselecting
  | SelectOrDrag (Index i) Position
  | Selecting (Index i)
  | Adding (IndexSet i) (Index i)
  | Removing (IndexSet i) (Index i)

type Selection i =
  Selection
    { style : Style
    , mode : Mode i
    , selection : IndexSet i
    , active : Bool
    , action : Action i
    }

init : Style -> Selection i
init style =
  Selection
    { style = style
    , mode = Hover
    , selection = IndexSet.empty
    , active = False
    , action = NoAction
    }

initSelectOnly : Selection i
initSelectOnly = init SelectOnly

initDraggable : Selection i
initDraggable = init SelectWithDrag

initExtendable : Selection i
initExtendable = init SelectExtend


normalize : Selection i -> Selection i
normalize ((Selection s) as sel) =
  if IndexSet.isEmpty s.selection
    then init s.style
    else sel

isSelected : Index i -> Selection i -> Bool
isSelected i (Selection s) =
  IndexSet.member i s.selection

anySelected : Selection i -> Bool
anySelected (Selection s) = not <| IndexSet.isEmpty s.selection

select : IndexSet i -> Selection i -> Selection i
select indexes (Selection s) =
  Selection
    { s
    | mode = Select
    , selection = indexes
    , active = False
    , action = NoAction
    }

selected : Selection i -> IndexSet i
selected (Selection s) = s.selection

firstSelected : Selection i -> Maybe (Index i)
firstSelected (Selection s) =
  case s.mode of
    Select ->
        case IndexSet.toList s.selection of
          first :: _ -> Just first
          _ -> Nothing
    _ -> Nothing

isDragging : Selection i -> Bool
isDragging (Selection s) =
  case s.mode of
    Drag xy _ -> s.active
    _ -> False

couldDrag : Selection i -> Bool
couldDrag (Selection s) =
  case s.mode of
    Select ->
      case s.action of
        SelectOrDrag _ _ -> s.active
        _ -> False
    Drag xy _ -> s.active
    _ -> False


type alias DragInfo i =
  { pos : Position, srcs : IndexSet i, dst : Maybe (Index i) }

dragInfo : Selection i -> Maybe (DragInfo i)
dragInfo (Selection s) =
  case s.mode of
    Drag xy mt -> Just { pos = xy , srcs = s.selection , dst = mt }
    _ -> Nothing

type alias DropInfo i = { srcs : IndexSet i, dst : Index i }

dropInfo : Selection i -> Maybe (DropInfo i)
dropInfo (Selection s) =
  case s.mode of
    Drop t -> Just { srcs = s.selection, dst = t }
    _ -> Nothing




act : Action i -> Index i -> IndexSet i -> IndexSet i
act action (Index j) s =
  let
    items (Index i) =
      IndexSet.fromList
      <| List.map Index
      <| List.range (min i j) (max i j)
  in
    case action of
      NoAction -> s
      Deselecting -> IndexSet.empty
      SelectOrDrag _ _ -> s
      Selecting i -> items i
      Adding basis i -> IndexSet.union basis (items i)
      Removing basis i -> IndexSet.diff basis (items i)

delayedAct : Action i -> IndexSet i -> IndexSet i
delayedAct action s =
  case action of
    SelectOrDrag i _ -> IndexSet.singleton i
    _ -> s

settleAction : Action i -> Action i
settleAction action =
  case action of
    SelectOrDrag i _ -> Selecting i
    _ -> action


type alias SelectionUpdater i = Selection i -> Selection i


mouseDown : Index i -> Bool -> MouseEvent -> SelectionUpdater i
mouseDown i occupied event ((Selection s) as sel) =
  let
    alreadySelected = s.mode == Select && isSelected i sel
    basis = s.selection
    extend = (if alreadySelected then Removing else Adding) basis i

    action =
      case (event.meta, event.shift) of
        (True, _) -> extend
        (False, True) -> s.action
        (False, False) ->
          if occupied
            then
              case s.style of
                SelectOnly -> Selecting i
                SelectWithDrag -> SelectOrDrag i event.xy
                SelectExtend -> extend
            else
              case s.style of
                SelectExtend -> NoAction
                _ -> Deselecting

    wait =
      case action of
        SelectOrDrag _ _ -> alreadySelected
          -- Don't do anything yet, in case it is the start of the drag.
          -- If it isn't, the action will be applied on mouse up.
        _ -> False

    selection =
      if wait
        then s.selection
        else act (settleAction action) i basis
  in
    Selection                 -- do not normalize here!
      { s
      | mode = Select
      , selection = selection
      , active = True
      , action = action
      }


{-

You might think mouseUp on each individual item would be a good idea....
...You'd be wrong!

The problem is that if some other part of the view has mouseUp handlers (say
another bank), and if the user slides over there and then releases the mouse,
the mouseUp will go there, and this bank will never see an end to the mouse
event. Sigh! So - better to just grab the global mouseup event.

Therefore, this code relies on mouseEnter/Leave to track the last valid (or not)
place the mouse was, and so to know where the user released the mosue.

mouseUp : Index i -> Bool -> MouseEvent -> SelectionUpdater i
mouseUp i occupied event ((Selection s) as sel) =
  case s.mode of
    Select ->
      let
        action = settleAction s.action
        sel_ =
          Selection
            { s
            | mode = Select
            , selection = act action i s.selection
            , active = False
            , action = action
            }
      in
        normalize sel_
    Drag _ _ ->
      Selection  { s | mode = Drop i, active = False, action = NoAction }
    _ -> Selection { s | active = False }
-}

mouseUpGlobal : SelectionUpdater i
mouseUpGlobal (Selection s) =
  case s.mode of
    Select ->
      normalize
      <| Selection
        { s
        | selection = delayedAct s.action s.selection
        , active = False
        , action = settleAction s.action
        }
    Drag _ mt ->
      Selection
        { s
        | mode = Maybe.unwrap Select Drop mt
        , active = False
        , action = NoAction
        }
    _ -> Selection { s | active = False }

mouseEnter : Index i -> Bool -> SelectionUpdater i
mouseEnter i occupied ((Selection s) as sel) =
  case s.mode of
    Hover ->
      if occupied
        then
          Selection
            { s
            | mode = Hover
            , selection = IndexSet.singleton i
            , active = False
            , action = NoAction
            }
        else sel
    Select ->
      if s.active
        then Selection { s | selection = act s.action i s.selection }
        else sel
    Drag xy _ ->
      Selection { s | mode = Drag xy (Just i) }
    _ -> sel

mouseLeave : Index i -> Bool -> SelectionUpdater i
mouseLeave i occupied ((Selection s) as sel) =
  case s.mode of
    Hover -> init s.style
    Drag xy _ ->
      Selection { s | mode = Drag xy Nothing }
    _ -> sel

mouseUpdate : MouseEvent -> SelectionUpdater i
mouseUpdate event ((Selection s) as sel) =
  let
    distanceSquared a b =
      (a.x - b.x) ^ 2 + (a.y - b.y) ^ 2
  in
    case s.mode of
      Select ->
        case s.action of
          SelectOrDrag _ p0 ->
            if s.active && distanceSquared p0 event.xy > 9
              then
                Selection
                  { s
                  | mode = Drag event.xy Nothing
                  , active = True
                  , action = settleAction s.action
                  }
              else sel
          _ -> sel
      Drag _ mt ->
        Selection { s | mode = Drag event.xy mt }
      _ -> sel



type Msg i
  = MouseDownOn (Index i) Bool MouseEvent
  | MouseEnter (Index i) Bool
  | MouseLeave (Index i) Bool
  | MouseUpdate MouseEvent
  | MouseUpGlobal

isMouseDown : Msg i -> Bool
isMouseDown msg =
  case msg of
  MouseDownOn _ _ _ -> True
  _ -> False

update : Msg i -> Selection i -> Selection i
update msg =
  case msg of
    MouseDownOn i occupied event  -> mouseDown i occupied event
    MouseEnter i occupied         -> mouseEnter i occupied
    MouseLeave i occupied         -> mouseLeave i occupied
    MouseUpGlobal                 -> mouseUpGlobal
    MouseUpdate event             -> mouseUpdate event

itemHandlers : Index i -> Bool -> List (Html.Attribute (Msg i))
itemHandlers i occupied =
  [ Events.onMouseDownExtended (MouseDownOn i occupied)
  , Events.onMouseEnter (MouseEnter i occupied)
  , Events.onMouseLeave (MouseLeave i occupied)
  ]

subscriptions : Selection i -> Sub (Msg i)
subscriptions (Selection s as sel) =
  if couldDrag sel
    then
      Sub.batch
      [ Browser.Events.onMouseMove (D.map MouseUpdate Events.mouseEvent)
      , Browser.Events.onMouseUp (D.succeed MouseUpGlobal)
      ]
    else
      if s.active
        then Browser.Events.onMouseUp (D.succeed MouseUpGlobal)
        else Sub.none



