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
  )

import Json.Decode as D

import Alert
import ByteArray exposing (ByteArray)
import Elektron.Digitakt.HighLevel as DT
import Elektron.Digitakt.Related as DT
import Elektron.Instrument as EI
import Job
import Missing.Time as Time
import Progress
import Project.Import as Import
import Project.Selection.Project as Sel
import Project.Util exposing (..)
import SysEx.Client
import SysEx.Dump
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
  , project : DT.Project
  , projectName : String
  , pendingReceive : PendingReceive
  , aboutToWriteFile : Bool
  , selection : Sel.Selection
  , nameEditing : Bool
  , related : DT.Related
  , importing : Maybe Import.Model
  , alert : Alert.Model
  , progress : Progress.Progress
  , undoStack : Undo.Model Checkpoint
  }


init : EI.Instrument -> Model
init instrument =
  let
    project = DT.emptyProject instrument
    selection = Sel.initSelection
  in
    { instrument = instrument
    , project = project
    , projectName = "DT Project"
    , pendingReceive = NothingPending
    , aboutToWriteFile = False
    , selection = selection
    , nameEditing = False
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
  | DeleteItems Kind

  | SelectionItemMsg Kind Int Sel.Msg
  | SelectionGlobalMsg Sel.Msg

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

