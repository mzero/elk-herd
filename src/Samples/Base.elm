module Samples.Base exposing
  ( ViewMode(..)
  , isPathSelected
  , isPathFocused
  , isPathFocusParent
  , pathPendingEdit
  , pathRelevantSelection
  , selectedPaths
  , focusedPath
  , focusedDirPath
  , selectIsDir
  , mkSelect

  , MouseMod(..)
  , MouseState(..)
  , dragging
  , updateDragPosition

  , SelectedPaths
  , Selection
  , InDrop

  , QueueAction(..)
  , Queue
  , emptyQueue

  , Model
  , init

  , Msg(..)

  , renameInputId
  )

import Dict
import Json.Decode as D
import Set

import Alert
import Elektron.Drive as Drive exposing (Drive)
import Elektron.Path as Path exposing (Path)
import Missing.Html.Events as Events
import Samples.Transfer exposing (Pump)
import SysEx.Message exposing (ElkMessage)



type ViewMode = ViewHover | ViewSelected | ViewAll

type MouseMod = Selecting | Extending | Adding | Removing

type MouseState
  = NoInteraction
  | Clicking MouseMod Events.Position Path
  | Dragging Events.Position Events.Position
  | Moving


dragging : MouseState -> Bool
dragging ms = case ms of
  Dragging _ _ -> True
  Moving -> True
  _ -> False

updateDragPosition : Events.Position -> MouseState -> MouseState
updateDragPosition xy ms = case ms of
  Clicking _ p0 _ ->
    let
      dx = xy.x - p0.x
      dy = xy.y - p0.y
      r2 = dx * dx + dy * dy
    in
      if r2 > 9
        then Dragging p0 xy
        else ms
  Dragging p0 _ -> Dragging p0 xy
  _ -> ms




type alias SelectedPaths =
  { paths : Set.Set Path      -- the set of selected items
  , basis : Set.Set Path      -- the set used when shift-selecting
  , focus : Path
  , isDir : Bool
  }

selectedPathsSingleton : Path -> Bool -> SelectedPaths
selectedPathsSingleton p isDir =
  let
    s1 = Set.singleton p
  in
    SelectedPaths s1 s1 p isDir

type alias Selection =
  { what : SelectedPaths
  , pendingEdit : Maybe String
  , mouseState : MouseState
  }


isPathSelected : Path -> Selection -> Bool
isPathSelected p sel = Set.member p sel.what.paths

isPathFocused : Path -> Selection -> Bool
isPathFocused p sel = p == sel.what.focus

isPathFocusParent : Path -> Selection -> Bool
isPathFocusParent p sel =
  not sel.what.isDir && p == Path.dirPath sel.what.focus

pathPendingEdit : Path -> Selection -> Maybe String
pathPendingEdit p sel =
  if p == sel.what.focus then sel.pendingEdit else Nothing

pathRelevantSelection : Path -> Maybe Selection -> Maybe Selection
pathRelevantSelection p = Maybe.andThen <| \sel ->
  if List.any (Path.startsWith p) (Set.toList sel.what.paths)
    then Just sel
    else Nothing

selectedPaths : Selection -> Set.Set Path
selectedPaths sel = sel.what.paths

focusedPath : Selection -> Path
focusedPath sel = sel.what.focus

focusedDirPath : Selection -> Path
focusedDirPath sel =
  (if sel.what.isDir then identity else Path.dirPath) sel.what.focus

selectIsDir : Selection -> Bool
selectIsDir sel = sel.what.isDir

mkSelect : Path -> Bool -> Bool -> MouseState -> Selection
mkSelect path isDir edit ms =
  let
    pr = if edit
      then Just <| Path.baseName path
      else Nothing
  in
    Selection (selectedPathsSingleton path isDir) pr ms

type alias InDrop =
  { target: Path
  , previousSelection : Maybe Selection
  }

type QueueAction
  = QAListDir Bool Path
  | QACreateDir Path
  | QADeleteDir Path
  | QADeleteFile Path
  | QARenameItem Path Path
  | QAReadFile Path String
  | QAWriteFile Path String D.Value

type alias Queue =
  { actions: List QueueAction
  , inFlight: Maybe QueueAction
  , started: Int
  , modal: Maybe (String, Bool)
  , scanAfter : List Path
  , selectAfter : Maybe { goodSel : Maybe Path
                        , badSel : Maybe Path
                        , isDir : Bool
                        , edit: Bool
                        }
  , badSamples  : List (String, String)
  }

emptyQueue : Queue
emptyQueue =
  { actions = []
  , inFlight = Nothing
  , started = 0
  , modal = Nothing
  , scanAfter = []
  , selectAfter = Nothing
  , badSamples = []
  }


type alias Model =
  { debug : Bool
  , opCounts : Dict.Dict String Int

  , drive : Drive
  , queue : Maybe Queue
  , pump : Maybe Pump
  , reportScan : Bool

  , selection : Maybe Selection
  , inDrop : Maybe InDrop
  , viewMode : ViewMode
  , alertModel : Alert.Model
  }

init : Bool -> Model
init debug =
  { debug = debug
  , opCounts = Dict.empty
  , drive = Drive.emptyDrive
  , queue = Nothing
  , pump = Nothing
  , reportScan = True
  , selection = Nothing
  , inDrop = Nothing
  , viewMode = ViewHover
  , alertModel = Alert.noAlert
  }


type Msg
  = KickOff
  | RecvLsResponse Bool Path ElkMessage
  | RecvResponse (Bool -> ElkMessage) ElkMessage
  | RecvPumpResponse ElkMessage

  | SetView ViewMode

  | MouseDownOn Path Bool Events.MouseEvent
  | MouseUpOn Path Bool Events.MouseEvent
  | MouseUpdate Events.Position
  | MouseUpGlobal

  | HoverMouseEnter Path Bool
  | HoverMouseLeave Path Bool

  | DragEnter Path
  | DragLeave Path

  | EditSelected String
  | BeginRename
  | CommitRename
  | CancelRename

  | BeginMove

  | CreateDir

  | MoveToTrash
  | EmptyTrash

  | FindDuplicates

  | CancelQueue

  | DroppedFile (List String, D.Value)
  | SendSampleFiles (List (String, D.Value))
  | SampleData (List (List Int))
  | SampleDataError String
  | ReceiveSampleFiles

  | WindowKey Int
  | OnAlert Alert.Msg

  | ReportOps
  | Ignore


renameInputId : String
renameInputId = "rename-input"
