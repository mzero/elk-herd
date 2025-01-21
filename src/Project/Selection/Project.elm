module Project.Selection.Project exposing
  ( Selection
  , initSelection

  , selectPatterns
  , selectSamples
  , selectSounds

  , selectedKind
  , selectedPatterns
  , selectedSamples
  , selectedSounds

  , firstSelected

  , DragInfo
  , dragInfo

  , DropInfo
  , dropInfo

  , selectRelated
  , computeRelated

  , Status(..)
  , kindStatus
  , itemStatus

  , Msg
  , isMouseDown
  , update
  , itemHandlers
  , subscriptions
  )

{-| Manages that there can be only one kind of thing selected in a project
at a time.
-}

import Html
import Html.Attributes as Attr

import Bank exposing (Index(..))
import Bank.IndexSet as IndexSet
import Elektron.Digitakt.HighLevel as DT
import Elektron.Digitakt.Related as DT
import Elektron.Digitakt.Types as DT
import Missing.Html.Events exposing (MouseEvent, Position)
import Missing.Maybe as Maybe
import Project.Selection.Bank as BSel
import Project.Util exposing (..)


type Selection
  = SNone
  | SPattern (BSel.Selection DT.Pattern)
  | SSample (BSel.Selection DT.Sample)
  | SSound (BSel.Selection DT.Sound)

initSelection : Selection
initSelection = SNone

normalize : Selection -> Selection
normalize s =
  let
    anySelected =
      case s of
        SNone -> True
        SPattern bs -> BSel.anySelected bs
        SSample bs -> BSel.anySelected bs
        SSound bs -> BSel.anySelected bs
  in
    if anySelected then s else SNone


isSelected : Kind -> Int -> Selection -> Bool
isSelected k i s =
  case (k, s) of
    (KPattern, SPattern bs) -> BSel.isSelected (Index i) bs
    (KSample, SSample bs) -> BSel.isSelected (Index i) bs
    (KSound, SSound bs) -> BSel.isSelected (Index i) bs
    _ -> False

deselect : Selection
deselect = initSelection


selectPatterns : List (Index DT.Pattern) -> Selection
selectPatterns indexes =
  SPattern <| BSel.select (IndexSet.fromList indexes) BSel.initDraggable

selectSamples : List (Index DT.Sample) -> Selection
selectSamples indexes =
  SSample <| BSel.select (IndexSet.fromList indexes) BSel.initDraggable

selectSounds : List (Index DT.Sound) -> Selection
selectSounds indexes =
  SSound <| BSel.select (IndexSet.fromList indexes) BSel.initDraggable

selectedKind : Selection -> Maybe Kind
selectedKind s =
  case s of
    SNone -> Nothing
    SPattern _ -> Just KPattern
    SSample _ -> Just KSample
    SSound _ -> Just KSound

selectedPatterns : Selection -> List (Index DT.Pattern)
selectedPatterns s =
  case s of
    SPattern bs -> BSel.selected bs |> IndexSet.toList
    _ -> []

selectedSamples : Selection -> List (Index DT.Sample)
selectedSamples s =
  case s of
    SSample bs -> BSel.selected bs |> IndexSet.toList
    _ -> []

selectedSounds : Selection -> List (Index DT.Sound)
selectedSounds s =
  case s of
    SSound bs -> BSel.selected bs |> IndexSet.toList
    _ -> []


firstSelected : Selection -> Maybe (Kind, Int)
firstSelected s =
  let
    check k bs =
      BSel.firstSelected bs
      |> Maybe.map (\i -> (k, Bank.indexToInt i))
  in
    case s of
      SNone -> Nothing
      SPattern bs -> check KPattern bs
      SSample bs -> check KSample bs
      SSound bs -> check KSound bs


isDragging : Selection -> Bool
isDragging s =
  case s of
    SNone -> False
    SPattern bs -> BSel.isDragging bs
    SSample bs -> BSel.isDragging bs
    SSound bs -> BSel.isDragging bs


dragDropSources : Kind -> IndexSet.IndexSet a -> List Int
dragDropSources k =
  IndexSet.toList
  >> List.map Bank.indexToInt
  >> if k == KSample
    then List.filter (Index >> DT.isZeroSampleIndex >> not)
    else identity

dragDropDestination : Kind -> Maybe (Index a) -> Maybe Int
dragDropDestination k =
  Maybe.andThen
    ( Bank.indexToInt
      >> if k == KSample
          then (\i -> if DT.isZeroSampleIndex (Index i) then Nothing else Just i)
          else Just
    )

type alias DragInfo =
  { pos : Position, kind : Kind, srcs : List Int, dst : Maybe Int }

dragInfo : Kind -> Selection -> Maybe DragInfo
dragInfo k s =
  let
    unwrapDragInfo { pos, srcs, dst } =
      { pos = pos
      , kind = k
      , srcs = dragDropSources k srcs
      , dst = dragDropDestination k dst
      }
    getDragInfo bs = Maybe.map unwrapDragInfo <| BSel.dragInfo bs
  in
    case (k, s) of
      (KPattern, SPattern bs) -> getDragInfo bs
      (KSample, SSample bs) -> getDragInfo bs
      (KSound, SSound bs) -> getDragInfo bs
      _ -> Nothing


type alias DropInfo = { kind : Kind, srcs : List Int, dst: Maybe Int }

dropInfo : Selection -> Maybe DropInfo
dropInfo s =
  let
    unwrap k { srcs, dst } =
      { kind = k
      , srcs = dragDropSources k srcs
      , dst = dragDropDestination k (Just dst)
      }
  in
    case s of
      SNone -> Nothing
      SPattern bs -> BSel.dropInfo bs |> Maybe.map (unwrap KPattern)
      SSample bs -> BSel.dropInfo bs |> Maybe.map (unwrap KSample)
      SSound bs -> BSel.dropInfo bs |> Maybe.map (unwrap KSound)




computeRelated : DT.CrossReference -> Selection -> DT.Related
computeRelated cr s =
  let
    related fn bs =
      BSel.selected bs
      |> IndexSet.toList
      |> List.map fn
      |> DT.mergeRelations
  in
    case s of
      SNone -> DT.noRelations
      SPattern bs -> related (DT.patternRelated cr) bs
      SSample bs -> related (DT.sampleRelated cr) bs
      SSound bs -> related (DT.soundRelated cr) bs

selectRelated : Kind -> DT.Related -> Selection
selectRelated k r =
  let
    build c is = c <| BSel.select (IndexSet.fromList is) BSel.initDraggable
    removeZero = List.filter ((/=) (Index 0))
  in
    normalize
    <| case k of
        KPattern -> build SPattern <| DT.relatedPatterns r
        KSample -> build SSample <| removeZero <| DT.relatedSamples r
          -- sample slot zero never moves, so don't both selecting it even if
          -- it is related to the current item
        KSound -> build SSound <| DT.relatedSounds r



type Status = Plain | Selected | Dragged | Related

kindStatus : Kind -> Selection -> Status
kindStatus k s =
  let
    bselStat bs =
      if BSel.isDragging bs then Dragged else Selected
  in
    case (k, s) of
      (_, SNone) -> Plain
      (KPattern, SPattern bs) -> bselStat bs
      (KSample, SSample bs) -> bselStat bs
      (KSound, SSound bs) -> bselStat bs
      _ -> Related

itemStatus : Kind -> Int -> Selection -> DT.Related -> Status
itemStatus k i s rel =
  case kindStatus k s of
    Plain -> Plain
    Selected -> if isSelected k i s then Selected else Plain
    Dragged -> if isSelected k i s then Dragged else Plain
    Related -> if isRelated k i rel then Related else Plain



type Msg
  = PatternMsg (BSel.Msg DT.Pattern)
  | SampleMsg (BSel.Msg DT.Sample)
  | SoundMsg (BSel.Msg DT.Sound)

isMouseDown : Msg -> Bool
isMouseDown msg =
  case msg of
    PatternMsg bmsg -> BSel.isMouseDown bmsg
    SampleMsg bmsg -> BSel.isMouseDown bmsg
    SoundMsg bmsg -> BSel.isMouseDown bmsg

update : Msg -> Selection -> Selection
update msg s =
  let
    updateMatch sType bmsg bs = sType <| BSel.update bmsg bs
    updateOther sType bmsg =
      if s == SNone || BSel.isMouseDown bmsg
        then sType <| BSel.update bmsg BSel.initDraggable
        else s
  in
    normalize
    <| case (msg, s) of
      (PatternMsg bmsg, SPattern bs) -> updateMatch SPattern bmsg bs
      (SampleMsg bmsg, SSample bs) -> updateMatch SSample bmsg bs
      (SoundMsg bmsg, SSound bs) -> updateMatch SSound bmsg bs
      (PatternMsg bmsg, _) -> updateOther SPattern bmsg
      (SampleMsg bmsg, _) -> updateOther SSample bmsg
      (SoundMsg bmsg, _) -> updateOther SSound bmsg

itemHandlers : Kind -> Int -> Bool -> List (Html.Attribute Msg)
itemHandlers k i occupied =
  let
    build mFn =
      List.map (Attr.map mFn) <| BSel.itemHandlers (Index i) occupied
  in
    case k of
      KPattern -> build PatternMsg
      KSample -> build SampleMsg
      KSound -> build SoundMsg

subscriptions : Selection -> Sub Msg
subscriptions s =
  case s of
    SNone -> Sub.none
    SPattern bs -> Sub.map PatternMsg <| BSel.subscriptions bs
    SSample bs -> Sub.map SampleMsg <| BSel.subscriptions bs
    SSound bs -> Sub.map SoundMsg <| BSel.subscriptions bs


