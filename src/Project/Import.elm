module Project.Import exposing
  ( Model
  , init

  , Msg
  , ImportUpdate(..)
  , update
  , view
  , subscriptions
  )

import Html
import Html.Attributes as Attr
import Html.Events as Events

import Bank exposing (Index(..), Bank, BankOf)
import Bank.Shuffle as Shuffle exposing (Shuffle)
import Bank.IndexSet as IndexSet exposing (IndexSet)
import Elektron.Digitakt.HighLevel as DT exposing (Project, Shuffles)
import Elektron.Digitakt.Related as Rel
import Elektron.Digitakt.Types as DT exposing (Pattern, Sample, Sound)
import Html.Aria as Aria
import Missing.Maybe as Maybe
import Project.Selection.Bank as BSel
import Project.Selection.Import as Sel
import Project.Util exposing (..)


type alias Items =
  { patterns : IndexSet Pattern
  , samples : IndexSet Sample
  , sounds : IndexSet Sound
  }

noItems : Items
noItems = Items IndexSet.empty IndexSet.empty IndexSet.empty


type alias Model =
  { origin : String

  , baseProject : Project
  , importProject : Project

  , baseFree : Items

  , existingItems : Items
  , existingShuffles : Shuffles

  , selection : Sel.Selection

  , selected : Items
  , referenced : Items
  , required : Items
  , needed : Items

  , importable : Bool
  }


init : String -> Project -> Project -> Model
init origin base source =
  let
    (existingItems, existingShuffles) = findExisting base source
  in
    { origin = origin
    , baseProject = base
    , importProject = source

    , baseFree = freeItems base

    , existingItems = existingItems
    , existingShuffles = existingShuffles

    , selection = Sel.initSelection

    , selected = noItems
    , referenced = noItems
    , required = noItems
    , needed = noItems

    , importable = False
    }


freeItems : Project -> Items
freeItems proj =
  let
    free (i, ma) =
      case ma of
        Just a -> if DT.isEmptyItem a then Just i else Nothing
        Nothing -> Just i

    find b =
      Bank.toIndexedList b
      |> List.filterMap free
      |> IndexSet.fromList
  in
    { patterns = find proj.patterns
    , samples =
        IndexSet.diff
          (find proj.samplePool)
          (IndexSet.singleton (Index 0))
    , sounds = find proj.soundPool
    }


findExisting : Project -> Project -> (Items, Shuffles)
findExisting baseProj sourceProj =
  let
    sampleShuf = DT.findSameSamples baseProj sourceProj
    soundShuf = DT.findSameSounds baseProj sourceProj

    items =
      { patterns = IndexSet.empty
      , samples = IndexSet.fromList <| Shuffle.shuffleSources sampleShuf
      , sounds = IndexSet.fromList <| Shuffle.shuffleSources soundShuf
      }

    shufs =
      { patterns = Shuffle.nullShuffle     -- patterns are never considered the same
      , samples = sampleShuf
      , sounds = soundShuf
      }
  in
    (items, shufs)


nonEmptySelection : BSel.Selection i -> Bank i (DT.BankItem a) -> IndexSet i
nonEmptySelection sel bank =
  let
    nonEmpty i =
      case Bank.get i bank of
        Nothing -> False
        Just a -> DT.isOccupiedItem a
  in
    BSel.selected sel |> IndexSet.filter nonEmpty

-- better to do all samples at once, then all sounds, then all patterns so that
-- relative order is preserved for those things being imported.

-- so need to compute full set of things to be moved first, then
-- move them



buildImportItems : Model -> Model
buildImportItems model =
  let
    selectedPatterns =
      nonEmptySelection model.selection.patterns model.importProject.patterns
    referencedPatterns = IndexSet.empty   -- nothing references patterns
    requiredPatterns = IndexSet.union selectedPatterns referencedPatterns
    existingPatterns = IndexSet.empty   -- patterns are always unique
    neededPatterns = IndexSet.diff requiredPatterns existingPatterns

    patternReferences =
      requiredPatterns
      |> IndexSet.toList
      |> List.filterMap
          (\i -> Bank.get i model.importProject.crossReference.patternRelated)
      |> Rel.mergeRelations


    selectedSounds =
      nonEmptySelection model.selection.sounds model.importProject.soundPool
    referencedSounds = IndexSet.fromList <| Rel.relatedSounds patternReferences
    requiredSounds = IndexSet.union selectedSounds referencedSounds
    existingSounds = model.existingItems.sounds
    neededSounds = IndexSet.diff requiredSounds existingSounds

    soundReferences =
      requiredSounds
      |> IndexSet.toList
      |> List.filterMap
          (\i -> Bank.get i model.importProject.crossReference.soundRelated)
      |> Rel.mergeRelations


    selectedSamples =
      nonEmptySelection model.selection.samples model.importProject.samplePool
    referencedSamples =
      IndexSet.union
        (IndexSet.fromList <| Rel.relatedSamples patternReferences)
        (IndexSet.fromList <| Rel.relatedSamples soundReferences)
    requiredSamples = IndexSet.union selectedSamples referencedSamples
    existingSamples = model.existingItems.samples
    neededSamples = IndexSet.diff requiredSamples existingSamples

  in
    checkImportable
    { model
    | selected = Items selectedPatterns selectedSamples selectedSounds
    , referenced = Items referencedPatterns referencedSamples referencedSounds
    , required = Items requiredPatterns requiredSamples requiredSounds
    , needed = Items neededPatterns neededSamples neededSounds
    }

checkImportable : Model -> Model
checkImportable model =
  let
    fits f =
      IndexSet.size (f model.baseFree) >= IndexSet.size (f model.needed)
  in
    { model
    | importable = fits .patterns && fits .samples && fits .sounds
    }


findBlock : Int -> IndexSet a -> IndexSet a
findBlock n idxs =
  let
    blockSize = 16
    blockStarts = List.map ((*) blockSize) <| List.range 0 7

    nBlocked = (n + blockSize - 1) // blockSize * blockSize

    blockSet s =
      IndexSet.fromList
      <| List.map Index
      <| List.map ((+) s)
      <| List.range 0 (nBlocked - 1)

    contiguous  i l =
      case l of
        [] -> [i]
        j :: _ -> if i == j + 1 then i :: l else [i]

    endSet =
      IndexSet.fromList
      <| List.map Index
      <| List.reverse
      <| List.foldl contiguous []
      <| List.map Bank.indexToInt
      <| IndexSet.toList idxs

    go bs =
      case bs of
        b :: rest ->
          if IndexSet.isSubset idxs (blockSet b)
            then blockSet b
            else go rest
        [] ->
          if IndexSet.size endSet >= n
            then endSet
            else idxs
  in
    go blockStarts

performImport : Model -> Project
performImport model =
  let
    buildShuffle : IndexSet a -> IndexSet a -> Shuffle a
    buildShuffle from to =
      Shuffle.asImport
        <| List.map2 Tuple.pair
          (IndexSet.toList from)
          (IndexSet.toList <| findBlock (IndexSet.size from) to)

    needed = model.needed
    baseFree = model.baseFree

    moving =
      Shuffles
        (buildShuffle needed.patterns baseFree.patterns)
        (buildShuffle needed.samples baseFree.samples)
        (buildShuffle needed.sounds baseFree.sounds)
  in
      DT.importProject moving model.existingShuffles
        model.importProject model.baseProject



type Msg
  = Cancel
  | Import
  | SelectionMsg Sel.Msg


type ImportUpdate
  = InProgress Model
  | Canceled
  | Imported String Project


update : Msg -> Model -> ImportUpdate
update msg model =
  case msg of
    Cancel -> Canceled
    Import -> Imported model.origin <| performImport model
    SelectionMsg selMsg ->
      InProgress
      <| buildImportItems
      <| { model | selection = Sel.update selMsg model.selection }




view : Model -> Html.Html Msg
view model =
  let
    isInItems : Kind -> Int -> Items -> Bool
    isInItems k i items =
      case k of
        KPattern -> IndexSet.member (Index i) items.patterns
        KSample -> IndexSet.member (Index i) items.samples
        KSound -> IndexSet.member (Index i) items.sounds

    countItems : Kind -> Items -> Int
    countItems k items =
      case k of
        KPattern -> IndexSet.size items.patterns
        KSample -> IndexSet.size items.samples
        KSound -> IndexSet.size items.sounds

    itemView : Kind -> Int -> Maybe (DT.BankItem a) -> Html.Html Msg
    itemView k i mItem =
      let
        name = Maybe.unwrap "" .name mItem
        empty = Maybe.unwrap True DT.isEmptyItem mItem
        phantom = Maybe.unwrap False DT.isPhantomItem mItem
        zero = k == KSample && DT.isZeroSampleIndex (Index i)
      in
        Html.div
          ( [ Attr.class "bank-item"
            , Attr.classList
              [ ("empty", empty)
              , ("zero", zero)
              , ("phantom", phantom)
              , ("selected",   isInItems k i model.selected)
              , ("referenced", isInItems k i model.referenced)
              , ("needed",     isInItems k i model.needed)
              ]
            ]
            ++ List.map
                (Attr.map SelectionMsg)
                (Sel.itemHandlers k i empty)
          )
          [ Html.span [ Attr.class "bank-label" ] [ Html.text (slotLabel k i) ]
          , Html.span [ Attr.class "bank-name" ]  [ Html.text name ]
          ]


    bankView : String -> String -> Kind -> BankOf (DT.BankItem a) -> Html.Html Msg
    bankView id label k bank =
      let
        column c =
          Html.div
            [ Attr.class "bank-column" ]
            <| List.map (\r -> item (c * 16 + r)) <| List.range 0 15
        columns = List.map column <| List.range 0 7
        item i = itemView k i (Bank.get (Index i) bank)

        selected = countItems k model.selected
        required = countItems k model.required
        needed = countItems k model.needed
        free = countItems k model.baseFree
        willFit = free >= needed

        counterText n s =
          Html.text
          <| if n == 0
              then "\u{00A0}"
              else String.padLeft 4 '\u{2007}' (String.fromInt n) ++ " " ++ s

      in
        Html.div
          [ Attr.class "section"
          , Attr.class id
          ]
          [ Html.div
            [ Attr.class "section-header" ]
            [ Html.h3
              [ Attr.classList [("text-danger", not willFit)] ]
              [ Html.text label ]
            , Html.p []
              [ counterText selected "selected"
              , Html.br [] []
              , counterText (required - selected) "addional needed"
              ]
            , Html.p []
              [ counterText needed "will be imported"
              , Html.br [] []
              , if willFit
                  then counterText (required - needed) "already in project"
                  else
                    Html.b [ Attr.class "text-danger" ]
                      [ counterText (needed - free) " more than will fit" ]
              ]
            ]
          , Html.div
            [ Attr.class "bank import" ]
            columns
          ]

    importHeader : Html.Html Msg
    importHeader =
      Html.div
        [ Attr.class "section"
        , Attr.class "import"
        ]
        [ Html.div
          [ Attr.class "section-header" ]
          [ Html.h3 [] [ Html.text "Importing..." ]
          , Html.p []
            [ Html.text """
Select the items to import to the project, then click Import.  Referenced
samples and sounds will also be imported, if needed.
"""       ]
          , Html.div
            [ Attr.class "btn-toolbar"
            , Aria.role "toolbar"
            ]
            [ Html.button
              [ Attr.class "btn btn-secondary"
              , Attr.type_ "button"
              , Events.onClick Cancel
              ]
              [ Html.text "Cancel" ]
            , Html.button
              [ Attr.class "btn btn-warning"
              , Attr.type_ "button"
              , Attr.disabled (not model.importable)
              , Events.onClick Import
              ]
              [ Html.text "Import" ]
            ]
          ]
        ]
  in
    Html.div
      [ ]
      [ importHeader
      , bankView "patterns" "Patterns"    KPattern model.importProject.patterns
      , bankView "samples"  "Sample Pool" KSample  model.importProject.samplePool
      , bankView "sounds"   "Sound Pool"  KSound   model.importProject.soundPool
      ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map SelectionMsg <| Sel.subscriptions model.selection
