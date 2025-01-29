module Project.Import exposing
  ( Model
  , init

  , Msg
  , ImportUpdate(..)
  , update
  , updateSampleNames
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
import Elektron.Drive as Drive
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
  , samplePoolOffset : Int

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
    , samplePoolOffset = 0

    , selected = noItems
    , referenced = noItems
    , required = noItems
    , needed = noItems

    , importable = False
    }


updateSampleNames : Drive.FileNamesByHash -> Model -> Model
updateSampleNames names model =
  { model
  | baseProject = DT.updateSampleNames names model.baseProject
  , importProject = DT.updateSampleNames names model.importProject
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
    , samples = find proj.samplePool
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


lastOccupied : Bank i (DT.BankItem a) -> Maybe Int
lastOccupied bank =
  Bank.toIndexedList bank
  |> List.foldl (\((Index i), ma) n ->
      case ma of
        Nothing -> n
        Just a -> if DT.isOccupiedItem a then Just i else n
    )
    Nothing

findDestination : Int -> Maybe Int -> IndexSet i -> IndexSet i
findDestination n last empties =
  let
    firstFree = Maybe.unwrap 0 (\i -> i + 1) last
    firstBlockFree = Maybe.unwrap 0 (\i -> (i + 16) // 16 * 16) last
    bigEnough start =
      let
        free = IndexSet.filter (\(Index i) -> i >= start) empties
      in
        if IndexSet.size free >= n then Just free else Nothing
  in
    bigEnough firstBlockFree
    |> Maybe.orElse (bigEnough firstFree)
    |> Maybe.withDefault empties


performImport : Model -> Project
performImport model =
  let
    buildShuffle : IndexSet a -> Maybe Int -> IndexSet a -> Shuffle a
    buildShuffle from last to =
      Shuffle.asImport
        <| List.map2 Tuple.pair
          (IndexSet.toList from)
          (IndexSet.toList <| findDestination (IndexSet.size from) last to)

    needed = model.needed
    baseFree = model.baseFree
    baseProject = model.baseProject

    moving =
      Shuffles
        (buildShuffle needed.patterns (lastOccupied baseProject.patterns) baseFree.patterns)
        (buildShuffle needed.samples (lastOccupied baseProject.samplePool) baseFree.samples)
        (buildShuffle needed.sounds (lastOccupied baseProject.soundPool) baseFree.sounds)
  in
      DT.importProject moving model.existingShuffles
        model.importProject model.baseProject



type Msg
  = Cancel
  | Import
  | SelectionMsg Sel.Msg
  | SetSamplePoolOffset Int


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
    SetSamplePoolOffset i ->
      InProgress { model | samplePoolOffset = i }




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
        occupied = Maybe.unwrap False DT.isOccupiedItem mItem
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
                (Sel.itemHandlers k i occupied)
          )
          [ Html.span [ Attr.class "bank-label" ] [ Html.text (slotLabel k i) ]
          , Html.span [ Attr.class "bank-name" ]  [ Html.text name ]
          ]


    bankSelector : Kind -> Html.Html Msg
    bankSelector k =
      let
        sampleBankButton n =
          let
            start = n * 128
            end = start + 127
            range = List.range start end
            itemOccupied i = Bank.get (Index i) model.importProject.samplePool
              |> Maybe.unwrap False DT.isOccupiedItem

            areAny f = List.any (\i -> isInItems k i (f model)) range
            empty = not <| List.any itemOccupied range
          in
            Html.button
              [ Attr.class "btn btn-light btn-sm"
              , Attr.classList
                  [ ("active", (model.samplePoolOffset == start))
                  , ("empty", empty)
                  , ("selected",   areAny .selected)
                  , ("referenced", areAny .referenced)
                  , ("needed",     areAny .needed)
                  ]
              , Attr.type_ "button"
              , Attr.disabled False
              , Events.onClick (SetSamplePoolOffset start)
              ]
              [ Html.text <| String.slice n (n + 1) "ABCDEFGHI" ]
      in
      case k of
        KSample ->
          if Bank.length model.importProject.samplePool > 128
            then
              Html.div
                [ Attr.class "btn-toolbar section-selector"
                , Aria.role "toolbar"
                , Attr.title "Bank Selector"
                , Attr.attribute "data-toggle" "tooltip"
                , Attr.attribute "data-placement" "right"
                ]
                ( List.map sampleBankButton <| List.range 0 7 )
            else Html.text ""
        _ -> Html.text ""

    bankView : String -> String -> Kind -> BankOf (DT.BankItem a) -> Int -> Html.Html Msg
    bankView id label k bank offset =
      let
        column c =
          Html.div
            [ Attr.class "bank-column" ]
            <| List.map (\r -> item (c * 16 + r + offset)) <| List.range 0 15
        columns = List.map column <| List.range 0 7
        item i = itemView k i (Bank.get (Index i) bank)

        selected = countItems k model.selected
        required = countItems k model.required
        needed = countItems k model.needed
        free = countItems k model.baseFree
        willFit = free >= needed

        counterText n z s =
          Html.text
          <| if n == 0 && not z
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
            , bankSelector k
            , Html.div [ Attr.class "status-bar"]
              [ Html.p []
                [ counterText selected (required > 0) "selected"
                , Html.br [] []
                , counterText (required - selected) False "addional needed"
                ]
              , Html.p []
                [ counterText needed (required > 0) "will be imported"
                , Html.br [] []
                , if willFit
                    then counterText (required - needed) False "already in project"
                    else
                      Html.b [ Attr.class "text-danger" ]
                        [ counterText (needed - free) False " more than will fit" ]
                ]
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
            [ Attr.class "btn-toolbar import-commands"
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
      , bankView "patterns" "Patterns"    KPattern model.importProject.patterns 0
      , bankView "samples"  "Sample Pool" KSample  model.importProject.samplePool model.samplePoolOffset
      , bankView "sounds"   "Sound Pool"  KSound   model.importProject.soundPool 0
      ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map SelectionMsg <| Sel.subscriptions model.selection
