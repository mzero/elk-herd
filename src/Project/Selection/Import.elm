module Project.Selection.Import exposing
  ( Selection
  , initSelection

  , isSelected

  , selectPatterns
  , selectSamples
  , selectSounds

  , selectedPatterns
  , selectedSamples
  , selectedSounds

  , computeRelated

  , Msg
  , isMouseDown
  , update
  , itemHandlers
  , subscriptions
  )

{-| During import, the user can select items from all three banks. So unlike
`Project.Selection.Project`, this module keeps track of all three selection
areas at once.
-}

import Html
import Html.Attributes as Attr

import Bank exposing (Index(..))
import Bank.IndexSet as IndexSet
import Elektron.Digitakt.Related as DT
import Elektron.Digitakt.Types as DT
import Project.Selection.Bank as BSel
import Project.Util exposing (..)


type alias Selection =
  { patterns : BSel.Selection DT.Pattern
  , samples : BSel.Selection DT.Sample
  , sounds : BSel.Selection DT.Sound
  }

initSelection : Selection
initSelection =
  Selection BSel.initExtendable BSel.initExtendable BSel.initExtendable

isSelected : Kind -> Int -> Selection -> Bool
isSelected k i s =
  case k of
    KPattern -> BSel.isSelected (Index i) s.patterns
    KSample -> BSel.isSelected (Index i) s.samples
    KSound -> BSel.isSelected (Index i) s.sounds

deselect : Selection
deselect = initSelection


selectPatterns : List (Index DT.Pattern) -> Selection -> Selection
selectPatterns indexes s =
  { s | patterns = BSel.select (IndexSet.fromList indexes) s.patterns }

selectSamples : List (Index DT.Sample) -> Selection -> Selection
selectSamples indexes s =
  { s | samples = BSel.select (IndexSet.fromList indexes) s.samples }

selectSounds : List (Index DT.Sound) -> Selection -> Selection
selectSounds indexes s =
  { s | sounds = BSel.select (IndexSet.fromList indexes) s.sounds }

selectedPatterns : Selection -> List (Index DT.Pattern)
selectedPatterns s =
  BSel.selected s.patterns |> IndexSet.toList

selectedSamples : Selection -> List (Index DT.Sample)
selectedSamples s =
  BSel.selected s.samples |> IndexSet.toList

selectedSounds : Selection -> List (Index DT.Sound)
selectedSounds s =
  BSel.selected s.sounds |> IndexSet.toList


computeRelated : DT.CrossReference -> Selection -> DT.Related
computeRelated cr s =
  let
    related fn bs =
      BSel.selected bs
      |> IndexSet.toList
      |> List.map fn
      |> DT.mergeRelations
  in
    DT.mergeRelations
      [ related (DT.patternRelated cr) s.patterns
      , related (DT.sampleRelated cr) s.samples
      , related (DT.soundRelated cr) s.sounds
      ]


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
  case msg of
    PatternMsg bmsg -> { s | patterns = BSel.update bmsg s.patterns }
    SampleMsg bmsg -> { s | samples = BSel.update bmsg s.samples }
    SoundMsg bmsg -> { s | sounds = BSel.update bmsg s.sounds }

itemHandlers : Kind -> Int -> Bool -> List (Html.Attribute Msg)
itemHandlers k i empty =
  let
    build mFn =
      List.map (Attr.map mFn) <| BSel.itemHandlers (Index i) empty
  in
    case k of
      KPattern -> build PatternMsg
      KSample -> build SampleMsg
      KSound -> build SoundMsg

subscriptions : Selection -> Sub Msg
subscriptions s =
  Sub.batch
    [ Sub.map PatternMsg <| BSel.subscriptions s.patterns
    , Sub.map SampleMsg <| BSel.subscriptions s.samples
    , Sub.map SoundMsg <| BSel.subscriptions s.sounds
    ]

