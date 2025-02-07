module Samples.Update exposing
  ( update
  , subscriptions
  )


import Browser.Dom as Dom
import Browser.Events
import Json.Decode as D
import Process
import Set
import Task
import Time

import Alert
import ByteArray exposing (ByteArray)
import Elektron.Drive as Drive exposing (Drive)
import Elektron.Instrument as EI
import Elektron.Path as Path exposing (Path)
import Missing.Html.Events as Events
import Missing.List as List
import Missing.Maybe as Maybe
import Missing.Time as Time
import Portage
import Report
import Samples.Base exposing (..)
import Samples.Ops as Ops
import Samples.Selection exposing (..)
import Samples.Transfer exposing (..)
import Samples.UpdateAction exposing (..)
import SysEx.Message as M exposing (ElkMessage)
import SysEx.Client exposing (..)
import Util
import WebMidi
import Windows1252


alwaysLog : String -> List String -> UpdateAction
alwaysLog msg values =
  addCmd
  <| Portage.log
  <| if List.isEmpty values
      then msg
      else msg ++ ": " ++ String.join ", " values

debugLog : String -> List String -> UpdateAction
debugLog msg values = withModel <| \model ->
  if model.debug
    then alwaysLog msg values
    else noop

alert : Alert.Level -> String -> String -> UpdateAction
alert level leadIn body =
  updateModel <| \model ->
    { model | alertModel = Alert.alert level leadIn body }

alertMarkdown : Alert.Level -> String -> UpdateAction
alertMarkdown level body =
  updateModel <| \model ->
    { model | alertModel = Alert.alertMarkdown level body }

clearAlert : UpdateAction
clearAlert = updateModel <| \model -> { model | alertModel = Alert.noAlert }

alertInfo : String -> String -> UpdateAction
alertInfo = alert Alert.Info

alertWarning : String -> String -> UpdateAction
alertWarning = alert Alert.Warning

alertDanger : String -> String -> UpdateAction
alertDanger = alert Alert.Danger


guardNotLocked : Path -> UpdateAction
guardNotLocked path = withModel <| \model ->
  let
    locked = case Drive.getEntry path model.drive of
      Just entry -> entry.locked
      Nothing -> False
  in
    if locked
      then
        alertWarning "Locked: " (Path.pathString path)
        >> done
      else
        noop

guardNotRoot : Path -> UpdateAction
guardNotRoot path =
  if path == Path.rootPath
    then
      alertWarning "Hands off: " "The top of +Drive cannot be renamed or moved."
      >> done
    else
      noop


updateQueue : (Queue -> Queue) -> UpdateAction
updateQueue f =
  updateModel <| \model ->
    { model | queue = Just <| f <| Maybe.withDefault emptyQueue model.queue }

enqueue : List QueueAction -> UpdateAction
enqueue qas =
  updateQueue (\q -> { q | actions = q.actions ++ qas })
  >> kickQueue

scanAfter : Path -> UpdateAction
scanAfter p = updateQueue <| \q -> { q | scanAfter = p :: q.scanAfter }

scanAllAfter : List Path -> UpdateAction
scanAllAfter ps = updateQueue <| \q -> { q | scanAfter = q.scanAfter ++ ps }

selectAfter : Maybe Path -> Maybe Path -> Bool -> Bool -> UpdateAction
selectAfter goodSel badSel isDir edit =
  updateQueue <| \q ->
    { q | selectAfter =
      Just { goodSel = goodSel, badSel = badSel, isDir = isDir, edit = edit }
    }

showProgress : String -> Bool -> UpdateAction
showProgress title cancelable =
  updateQueue <| \q -> { q | modal = Just (title, cancelable) }

bumpPump : (Pump, ElkMessage) -> UpdateAction
bumpPump (pump, msg) =
  updateModel (\m -> { m | pump = Just pump })
  >> addRequest msg RecvPumpResponse

endPump : UpdateAction
endPump =
  updateModel (\m -> { m | pump = Nothing })

userCanceledPump : UpdateAction
userCanceledPump =
  updateModel (\m -> { m | pump = Maybe.map (cancelPump "Canceled") m.pump  })

bumpQueue : UpdateAction
bumpQueue = withModel <| \model ->
  case model.queue of
    Nothing -> noop
    Just q -> case q.actions of
      [] -> finishQueue True
      act :: acts ->
        let
          q_ = { q | actions = acts, inFlight = Just act, started = q.started + 1 }

          uact = case act of
            QAListDir rec path ->
              addRequest
                (M.DirListRequest (Path.pathString path))
                (RecvLsResponse rec path)

            QACreateDir path ->
              addRequest
                (M.DirCreateRequest (Path.pathString path))
                (RecvResponse M.DirCreateResponse)

            QADeleteDir path ->
              addRequest
                (M.DirDeleteRequest (Path.pathString path))
                (RecvResponse M.DirDeleteResponse)

            QADeleteFile path ->
              addRequest
                (M.FileDeleteRequest (Path.pathString path))
                (RecvResponse M.FileDeleteResponse)

            QARenameItem src dst ->
              addRequest
                (M.ItemRenameRequest (Path.pathString src) (Path.pathString dst))
                (RecvResponse M.ItemRenameResponse)

            QAReadFile path name ->
              bumpPump <| startRead (Path.pathString path)
            QAWriteFile path fileName v ->
              addCmd <| Portage.readAudioFile (EI.supportsStereo model.instrument, v)
        in
          updateModel (\m -> { m | queue = Just q_ })
          >> uact

kickQueue : UpdateAction
kickQueue = withModel <| \model ->
  case model.queue of
    Just q -> if q.inFlight == Nothing then bumpQueue else noop
    Nothing -> noop

finishQueue : Bool -> UpdateAction
finishQueue good = withModel <| \model ->
  case model.queue of
    Nothing -> noop
    Just q ->
      updateModel (\m -> { m | queue = Nothing })
      >> (if q.scanAfter /= []
          then
            enqueue
              (List.map (QAListDir (not good)) <| List.nub q.scanAfter)
            >> updateQueue
              (\q2 -> { q2 | modal = Maybe.map (\(t,_) -> (t,False)) q.modal })
          else noop
          )
      >> (if List.isEmpty q.badSamples
          then noop
          else
            let
              mdHead = """
## Untransfered files:

The browser couldn't load samples from these files. Browsers only support a few
audio file formats, including: WAV, MP3, and FLAC.

"""
              mdItems =  String.join "\n" (List.map mdItem q.badSamples)
              mdItem (path, msg) = "* `" ++ path ++ "` - " ++ msg
            in
            alertMarkdown Alert.Info (mdHead ++ mdItems)
         )
      >> case q.selectAfter of
          Just sa ->
            let
              selPath = if good then sa.goodSel else sa.badSel
              selAction = case selPath of
                Nothing -> updateModel (\m -> { m | selection = Nothing })
                Just p -> setSelection <| mkSelect p sa.isDir (sa.edit && good) NoInteraction
            in
              selAction
               >> if sa.edit && good then focusOnInput else noop
          Nothing -> noop


recursiveMove : Path -> Path -> Drive -> List QueueAction
recursiveMove src0 dst0 drive =
  let
    mv e src dst = case Drive.subEntries e of
      Nothing -> [ QARenameItem src dst ]
      Just subs ->
        QACreateDir dst
        :: List.concatMap
          (\sub ->
            mv sub (Path.subPath src sub.name) (Path.subPath dst sub.name))
          subs
        ++ [ QADeleteDir src ]
  in
    case Drive.getEntry src0 drive of
      Nothing -> []
      Just e0 -> mv e0 src0 dst0

addDistinct : Path -> List Path -> List Path
addDistinct p qs =
  if List.any (\q -> Path.startsWith q p) qs
    then qs
    else p :: List.filter (\q -> not <| Path.startsWith p q) qs


moveSelection : Selection -> Path -> String -> Bool -> UpdateAction
moveSelection sel target title alwaysNext = withModel <| \model ->
  let
    isDir = selectIsDir sel
    pSrc = focusedPath sel
    pSrcs = selectedPaths sel
      |> Set.toList
      |> List.filter (\s -> Path.dirPath s /= target)
          -- remove anything already directly in target
      |> List.foldl addDistinct []
          -- remove sources that are covered by other sources
    bSrcs = List.map Path.baseName pSrcs

    bDsts = Drive.makeUniqueNames target bSrcs model.drive
    pDsts = List.map (Path.subPath target) bDsts

    moves = List.zip pSrcs pDsts
    moreThanOne = List.length moves > 1

    pSrcParents = Set.toList <| Set.fromList <| List.map Path.dirPath pSrcs

    (pSelNext, pSelBad, pSelIsDir) = case moves of
      [(s1, d1)] ->
        if not alwaysNext && isDir
          then (Just d1, Just s1, True)
          else case pathNext False s1 model.drive of
            Just _ as p -> (p, Just s1, isDir)
            Nothing -> (pathNext True pSrc model.drive, Just s1, isDir)
      _ -> (\s -> (s, Nothing, True)) <|
        if not alwaysNext && isDir
          then Just target
          else case pSrcParents of
            p0 :: pn -> Just <| List.foldl Path.deepestCommonAncestor p0 pn
            [] -> Nothing

    preCreate = case Drive.getEntry target model.drive of
        Just _ -> noop
        Nothing ->
          enqueue [QACreateDir target]
          >> scanAfter (Path.dirPath target)

    guardNotRecursive s = if Path.startsWith s target
        then
          alertWarning "Möbius: "
            ("Can't move " ++ Path.pathString s ++ " under itself.")
          >> done
        else
          noop
  in
    sequence (List.map guardNotRoot pSrcs)
    >> guard (List.all (\(s,d) -> s /= d) moves) -- should never happen
    >> sequence (List.map guardNotLocked pSrcs)
    >> sequence (List.map guardNotLocked pSrcParents)
    >> sequence (List.map guardNotRecursive pSrcs)
    >> guardNotLocked target
    >> preCreate
    >> enqueue (List.concatMap (\(s,d) -> recursiveMove s d model.drive) moves)
    >> scanAllAfter pSrcParents
    >> scanAfter target
    >> selectAfter pSelNext pSelBad pSelIsDir False
    >> if isDir || moreThanOne
        then showProgress title True
        else noop


focusOnDrive : UpdateAction
focusOnDrive = addCmd <| Task.attempt (always Ignore) (Dom.focus "drive")


focusOnInput : UpdateAction
focusOnInput = addCmd <| Task.attempt (always Ignore)
  -- delay to ensure that the view has been refreshed and the input created
  (  Process.sleep (200 * Time.millisecond)
  |> Task.andThen (\_ -> Dom.focus renameInputId)
  )


beginRename : UpdateAction
beginRename = withModel <| \model ->
  case model.selection of
    Just sel ->
        let
          pItem = focusedPath sel
          isDir = selectIsDir sel
          pDir = Path.dirPath pItem
        in
          guardNotRoot pItem
          >> guardNotLocked pItem
          >> guardNotLocked pDir
          >> setSelection (mkSelect pItem isDir True NoInteraction)
          >> focusOnInput
    _ -> noop

commitRename : UpdateAction
commitRename = withModel <| \model ->
 case model.selection of
    Just sel -> case sel.pendingEdit of
      Just newBase ->
        let
          isDir = selectIsDir sel
          pOld = focusedPath sel
          sOld = Path.baseName pOld
          sNew = Windows1252.fileNameClean newBase
          pDir = Path.dirPath pOld
          pNew = Path.subPath pDir sNew
          exists = case Drive.getEntry pNew model.drive of
            Nothing -> False
            Just _ -> True
        in
          setSelection { sel | pendingEdit = Nothing }
          >> guard (sNew /= "")
          >> guard (sNew /= sOld)
          >> guard (pOld /= Path.rootPath)
          >> if exists
            then alertWarning "Cannot Rename: " "There already is something named that."
            else
              Ops.bumpRename isDir
              >> enqueue [ QARenameItem pOld pNew ]
              >> scanAfter pDir
              >> selectAfter (Just pNew) (Just pOld) isDir False
      _ ->
        noop
    _ ->
      noop

cancelRename : UpdateAction
cancelRename = withModel <| \model ->
  case model.selection of
    Just sel -> setSelection { sel | pendingEdit = Nothing }
    Nothing -> noop


updateDrag : (MouseState -> MouseState) -> UpdateAction
updateDrag f = withModel <| \model ->
  case model.selection of
    Just sel -> setSelection { sel | mouseState = f sel.mouseState }
    Nothing -> noop

commitDrag : Selection -> Path -> UpdateAction
commitDrag sel target = withModel <| \model ->
  setSelection { sel | mouseState = NoInteraction }
  >> clearAlert
  >> moveSelection sel target "Move Items" False
  >> Ops.bumpMove (selectIsDir sel)

cancelDrag : Selection -> UpdateAction
cancelDrag sel =
  setSelection { sel | mouseState = NoInteraction }
  >> clearAlert


moveToTrash : UpdateAction
moveToTrash = withModel <| \model ->
  case model.selection of
    Just sel ->
      moveSelection sel Path.trashPath "Move to Trash" True
      >> Ops.bumpMoveToTrash
    Nothing -> noop

sendSampleFiles : Model -> List (List String, D.Value) -> UpdateAction
sendSampleFiles model items =
    let
      prepNames names = case List.reverse names of
        last :: rest -> List.reverse (prepSampleName last :: rest)
        [] -> []

      iNames = List.map (Tuple.first >> prepNames) items

      pBase = Maybe.unwrap Path.rootPath focusedDirPath model.selection

      iUniqFirsts = Drive.makeUniqueNames pBase
        (List.map (List.head >> Maybe.withDefault "sample") iNames)
        model.drive

      samplePath = List.foldl (\n p -> Path.subPath p n) pBase
      paths = List.map2 (\n u -> samplePath <| (u :: List.drop 1 n))
        iNames iUniqFirsts

      buildAction (names, file) path =
        if List.any skipSampleName names
          then Nothing
          else Just (QAWriteFile path (String.join "/" names) file)

      actions = List.map2 buildAction items paths
    in
      updateModel (\m -> { m | inDrop = Nothing })
      >> enqueue (List.filterMap identity actions)
      >> scanAfter pBase
      >> showProgress "Sending Samples" True
      >> Ops.bumpSendSamples

receiveSampleFiles : Selection -> UpdateAction
receiveSampleFiles sel =
  let
    items = if selectIsDir sel
      then []
      else List.map (\p -> (p, Path.baseName p))
        <| Set.toList <| selectedPaths sel
  in
    if List.isEmpty items
      then noop
      else
        enqueue (List.map (\(p, n) -> QAReadFile p n) items)
        >> showProgress "Receiving Samples" True
        >> Ops.bumpReceiveSamples

writeSamples : Model -> List ByteArray -> UpdateAction
writeSamples model bss =
  case model.queue |> Maybe.andThen .inFlight of
    Just (QAReadFile _ name) -> addCmd <| Portage.writeAudioFile name bss
    _ -> noop

updateRecvUnexpectedMsg : ElkMessage -> UpdateAction
updateRecvUnexpectedMsg msg = withModel <| \model ->
  let
    inFlight = model.queue |> Maybe.andThen .inFlight
  in
    finishQueue False
    >> alwaysLog "unexpected message"
      [ Maybe.unwrap "<no action>" queueActionLogString inFlight
      , M.messageLogString msg
      ]
    >> case msg of
      M.TimeOut ->
        updateModel (\m -> { m | drive = Drive.emptyDrive })
        >> alert Alert.Fatal "Communication Failed" """
The instrument did not respond after trying a few times.
If you have directories with many samples (>100) try adding "?slow" to the end
of the URL in the address bar and reloading this app. This will make the
application wait longer for your instrument to respond.
"""
      _ ->
        addCmd (WebMidi.genUnexpectedMessageError)

updateRecvResponseFail : ElkMessage -> UpdateAction
updateRecvResponseFail msg = withModel <| \model ->
  let
    inFlight = model.queue |> Maybe.andThen .inFlight
  in
    finishQueue False
    >> alwaysLog "failed message"
      [ Maybe.unwrap "<no action>" queueActionLogString inFlight
      , M.messageLogString msg
      ]
    >> alert Alert.Warning "Operation failed: "
      """
  The instrument wouldn't do what we asked. This can happen if you've edited
  the +Drive directly the instrument. Try re-scanning. If it happens again,
  tell us about it (see Credits in help).
  """


update : Msg -> Model -> (Model, Cmd Msg, Requests Msg)
update outerMsg model = run model <| case outerMsg of
  KickOff ->
    updateModel reset
    >> Ops.bumpScanDrive
    >> enqueue [ QAListDir True Path.rootPath ]
    >> showProgress "Scan +Drive" False
    >> addCmd (Report.startDriveScan)

  FindDuplicates ->
    let
      collisions = Drive.collisions model.drive
    in
      if List.isEmpty collisions
        then
          alertInfo "No file duplicates found." ":-)"
        else
          let
            mdHead = "## Sample duplicates:\n\n"
            mdBody =
              String.join "\n----\n\n" (List.map mdCollision collisions)
            mdCollision ((h, s), ps) =
              "### These samples are the same: "
              ++ "_(" ++ Util.siString s ++ ")_\n"
              ++ String.join "" (List.map mdPath ps)
            mdPath p = "    " ++ Path.pathString p ++ "\n"
          in
            alertMarkdown Alert.Info (mdHead ++ mdBody)

  RecvLsResponse rec path msg -> case msg of
    M.DirListResponse rawEntries ->
      let
        (drive_, entries) = Drive.setRawEntries rec path rawEntries model.drive
        subs = List.map .path <| List.filter Drive.isDirEntry entries
        emptyQueue = Maybe.unwrap True (List.isEmpty << .actions) model.queue
      in
        debugLog "scan response"
          [ Path.pathString path
          , String.fromInt (List.length rawEntries) ++ " entries"
          , String.fromInt (List.length subs) ++ " subs"
          ]
        >> updateModel (\m -> { m | drive = drive_ })
        >> (if List.isEmpty subs && emptyQueue
              then
                if model.reportScan
                  then
                    addCmd (Report.driveStats drive_)
                    >> updateModel (\m -> { m | reportScan = False })
                  else
                    noop
              else
                enqueue (List.map (QAListDir rec) subs))
        >> bumpQueue
    _ -> updateRecvUnexpectedMsg msg

  RecvResponse respMsgFn msg ->
    let
      match ok = if msg == respMsgFn ok then Just ok else Nothing
      check = case msg of
        M.DirCreateResponse ok -> match ok
        M.DirDeleteResponse ok -> match ok
        M.FileDeleteResponse ok -> match ok
        M.ItemRenameResponse ok -> match ok
        _ -> Nothing
    in
      case check of
        Nothing -> updateRecvUnexpectedMsg msg
        Just True -> bumpQueue
        Just False -> updateRecvResponseFail msg

  RecvPumpResponse msg ->
    case model.pump of
      Just p ->
        case pump p msg of
          Continue p_ req ->
            bumpPump (p_, req)
          ReadDone bss ->
            writeSamples model bss >> endPump >> bumpQueue
          WriteDone ->
            endPump >> bumpQueue
          Error userMessage ->
            endPump  -- should close up?
            >> finishQueue False
            >> alert Alert.Warning "Transfer failed: " userMessage
      Nothing -> noop

  SetView vm ->
    if vm == model.viewMode
      then noop
      else updateModel (\m -> { m | viewMode = vm, selection = Nothing })

  MouseDownOn p isDir evt ->
    if Maybe.unwrap False ((==) Moving << .mouseState) model.selection
      then noop
      else focusOnDrive
        >> updateModel (\m -> { m | viewMode = ViewSelected } )
        >> startMouse p isDir evt model.selection
  MouseUpdate xy -> updateDrag <| updateDragPosition xy
  MouseUpOn p isDir evt -> case model.selection of
    Just sel -> case sel.mouseState of
      NoInteraction -> noop
      Clicking _ _ _ -> finishMouse sel
      Dragging _ _ -> if isDir then commitDrag sel p else cancelDrag sel
      Moving -> if isDir then commitDrag sel p else cancelDrag sel
    Nothing -> noop
  MouseUpGlobal -> case model.selection of
    Just sel -> cancelDrag sel
    Nothing -> noop

  HoverMouseEnter p isDir ->
    setSelection <| mkSelect p isDir False NoInteraction
  HoverMouseLeave p isDir ->
    if (Maybe.map focusedPath model.selection) == Just p
      then updateModel (\m -> { m | selection = Nothing })
      else noop

  DragEnter p ->
    updateModel (\m ->
      { m | selection = Just <| mkSelect p True False NoInteraction
          , inDrop = Just <| case m.inDrop of
              Nothing -> InDrop p model.selection
              Just d -> { d | target = p }   -- retain the original selection
      })
  DragLeave p ->
    case model.inDrop of
      Just d -> if d.target == p
        then updateModel (\m ->
          { m | selection = d.previousSelection
              , inDrop = Nothing
          })
        else noop
      Nothing -> noop

  EditSelected s ->
    case model.selection of
      Just sel ->
        setSelection { sel | pendingEdit = Just (Windows1252.fileNameClean s) }
      Nothing -> noop
  BeginRename -> beginRename
  CommitRename -> commitRename
  CancelRename -> cancelRename

  BeginMove ->
    case model.selection of
      Just sel ->
        setSelection { sel | mouseState = Moving }
        >> alertInfo "Moving: "
          "Now click on a directory to move these items to."
      Nothing -> noop

  CreateDir ->
    let
      pBase = Maybe.unwrap Path.rootPath focusedDirPath model.selection
      sNew = Drive.makeUniqueName pBase "samples" model.drive
      pNew = Path.subPath pBase sNew
    in
      guardNotLocked pBase
      >> Ops.bumpCreateDir
      >> enqueue [ QACreateDir pNew ]
      >> scanAfter (Path.dirPath pNew)
      >> selectAfter (Just pNew) Nothing True True


  MoveToTrash -> moveToTrash
  EmptyTrash ->
    let
      mkAct e = if Drive.isDirEntry e
        then QADeleteDir e.path
        else QADeleteFile e.path
      actions = List.map mkAct
        <| Drive.contentEntriesDepthFirst Path.trashPath model.drive
    in
      addCmd (Report.trashStats model.drive)
      >> Ops.bumpEmptyTrash
      >> enqueue actions
      >> scanAfter Path.trashPath
      >> selectAfter Nothing (Just Path.trashPath) True False
      >> showProgress "Empty Trash" True

  CancelQueue ->
    userCanceledPump
    >> finishQueue False

  DroppedFile (names, v) ->
    sendSampleFiles model [(names, v)]

  SendSampleFiles items ->
    sendSampleFiles model <| List.map (Tuple.mapFirst List.singleton) items

  SampleData bytes -> case model.queue |> Maybe.andThen .inFlight of
    Just (QAWriteFile path _ _) ->
      bumpPump <| startWrite (Path.pathString path) bytes
    _ ->
      bumpQueue

  SampleDataError errorMessage -> case model.queue of
    Just q -> case q.inFlight of
      Just (QAWriteFile _ fileName _) ->
        updateQueue (\q_ ->
          { q_ | badSamples = (fileName, errorMessage) :: q_.badSamples })
        >> bumpQueue
      _ -> bumpQueue
    Nothing ->
      noop

  ReceiveSampleFiles ->
    case model.selection of
      Just sel -> receiveSampleFiles sel
      Nothing -> noop


  WindowKey kc -> case kc of
    0x08 -> moveToTrash           -- backspace/del
    0x0d -> beginRename           -- return
    0x25 -> Ops.bumpArrowKey >> selectParent          -- left
    0x26 -> Ops.bumpArrowKey >> selectSibling True    -- up
    0x27 -> Ops.bumpArrowKey >> selectChild           -- right
    0x28 -> Ops.bumpArrowKey >> selectSibling False   -- down
    _ -> noop

  OnAlert msg ->
    updateModel (\m -> { m | alertModel = Alert.update msg m.alertModel })

  ReportOps -> Ops.reportAndZero
  Ignore -> noop

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    interactSub = case model.selection of
      Just sel -> case sel.mouseState of
        NoInteraction -> if sel.pendingEdit == Nothing
          then Browser.Events.onKeyDown
            (D.map WindowKey (D.field "keyCode" D.int))
          else Sub.none
        _ -> Sub.batch [ Browser.Events.onMouseMove (D.map MouseUpdate Events.position)
                       , Browser.Events.onMouseUp (D.succeed MouseUpGlobal) ]
      _ -> Sub.none
  in
    Sub.batch
      [ interactSub
      , Portage.droppedFile DroppedFile
      , Portage.sampleData SampleData
      , Portage.sampleDataError SampleDataError
      , Sub.map OnAlert <| Alert.subscriptions model.alertModel
      , Ops.subscriptions
      ]

