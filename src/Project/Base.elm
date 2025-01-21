module Project.Base exposing
  ( Model
  , init

  , Disposition(..)
  , PendingReceive(..)
  , Checkpoint

  , Update
  , UpdateJob
  , Msg(..)

  , bankId
  , itemId
  , itemInputId

  , bankName
  , itemName

  , shuffleSpec, shuffleAllSpec
  )

import Dict
import Json.Decode as D

import Alert
import Bank exposing (Index(..))
import ByteArray exposing (ByteArray)
import Elektron.Digitakt.HighLevel as DT
import Elektron.Digitakt.Related as DT
import Elektron.Digitakt.Shuffle as Shuffle
import Elektron.Digitakt.Types as DT
import Elektron.Drive as Drive
import Elektron.Instrument as EI
import Job
import Missing.Time as Time
import Progress
import Project.Import as Import
import Project.Selection.Project as Sel
import Project.Util exposing (..)
import SysEx.Client
import SysEx.Dump
import SysEx.Message exposing (ElkMessage)
import Undo

{-| What to do with a project after we've received Import.
-}
type Disposition
  = LoadProject
  | StartImport

type PendingReceive
  = NothingPending
  | FromDevice Disposition DT.Project
  | FromFile Disposition String

type alias Checkpoint = (DT.Project, Sel.Selection)

type alias Model =
  { instrument : EI.Instrument
  , projectSpec : EI.ProjectSpec
  , project : DT.Project
  , projectName : String
  , extraFileNames : Drive.FileNamesByHash
  , pendingReceive : PendingReceive
  , aboutToWriteFile : Bool
  , selection : Sel.Selection
  , nameEditing : Bool
  , samplePoolOffset : Int
  , related : DT.Related
  , importing : Maybe Import.Model
  , alert : Alert.Model
  , progress : Progress.Progress
  , undoStack : Undo.Model Checkpoint
  }


init : EI.Instrument -> EI.ProjectSpec -> Model
init instrument projectSpec =
  let
    project = DT.emptyProject instrument.device projectSpec
    selection = Sel.initSelection
  in
    { instrument = instrument
    , projectSpec = projectSpec
    , project = project
    , projectName = "DT Project"
    , extraFileNames = Dict.empty
    , pendingReceive = NothingPending
    , aboutToWriteFile = False
    , selection = selection
    , nameEditing = False
    , samplePoolOffset = 0
    , related = DT.noRelations
    , importing = Nothing
    , alert = Alert.noAlert
    , progress = Progress.init
    , undoStack = Undo.init 10 (project, selection)
    }


type alias Update = (Model, Cmd Msg, SysEx.Client.Requests Msg)
type alias UpdateJob = Job.Job ((Int, Int), Time.Time, Cmd Msg) (Model -> Update)


type Msg
  = NoOp

  | ClearProject
  | RequestProject Disposition
  | ReceiveDump SysEx.Dump.ElkDump
  | ReceiveSampleFileInfo ElkMessage
  | SendProject

  | StartWriteProjectFile
  | UpdateProjectName String
  | CancelWriteProjectFile
  | WriteProjectFile

  | SelectProjectFile Disposition (List (String, D.Value))
  | ReadProjectFile ByteArray
  | CancelReadProjectFile String

  | StepJob UpdateJob

  | ImportMsg Import.Msg

  | SelectRelated Kind
  | SelectUnused Kind
  | RenameItem Kind
  | CompactItems Kind
  | SortItems Kind
  | DeleteItems Kind

  | SelectionItemMsg Kind Int Sel.Msg
  | SelectionGlobalMsg Sel.Msg

  | SetSamplePoolOffset Int

  | ChangeItemName Kind Int String
  | KeyDown Kind String

  | AlertMsg Alert.Msg
  | UndoMsg Undo.Msg


kindId : Kind -> String
kindId k =
  case k of
    KPattern -> "patterns"
    KSample -> "samples"
    KSound -> "sounds"

bankId : Kind -> String
bankId k = kindId k ++ "-bank"

itemId : Kind -> Int -> String
itemId k i =
  kindId k ++ "-item-" ++ String.fromInt i

itemInputId : Kind -> Int -> String
itemInputId k i = itemId k i ++ "-input"


bankName : Kind -> String
bankName k =
  case k of
    KPattern -> "Patterns"
    KSample -> "Sample Pool"
    KSound -> "Sound Pool"

kindName : Kind -> String
kindName k =
  case k of
    KPattern -> "Pattern"
    KSample -> "Sample"
    KSound -> "Sound"

itemName : Kind -> Int -> String
itemName k i = kindName k ++ " " ++ slotLabel k i


shuffleSpec : Kind -> Model -> Shuffle.Spec i (DT.BankItem a)
shuffleSpec k model =
  let
    start =
      case k of
        KPattern -> 0
        KSample -> model.samplePoolOffset
        KSound -> 0
  in
  { isEmpty = DT.isEmptyItem
  , skip =
      case k of
        KPattern -> always False
        KSample -> (\(Index i) -> modBy 128 i == 0)
        KSound -> always False
  , lowerBound = Index start
  , upperBound = Index (start + 127)
  }

shuffleAllSpec : Kind -> Model -> Shuffle.Spec i (DT.BankItem a)
shuffleAllSpec k model =
  let
    spec = shuffleSpec k model
    size =
      case k of
        KPattern -> Bank.length model.project.patterns
        KSample -> Bank.length model.project.samplePool
        KSound -> Bank.length model.project.soundPool
  in
    { spec | lowerBound = Index 0, upperBound = Index (size - 1) }